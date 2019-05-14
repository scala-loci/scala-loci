package loci.dev
package language
package impl
package components

import scala.collection.mutable
import scala.reflect.macros.blackbox

object RemoteAccess extends Component.Factory[RemoteAccess](
    requires = Seq(Commons, ModuleInfo, Peers, Values)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new RemoteAccess(engine)
  def asInstance[C <: blackbox.Context] = { case c: RemoteAccess[C] => c }

  object placedValueCache extends ContextReference {
    class Value[C <: blackbox.Context](val c: C) extends Value.Base[C] {
      val cache = mutable.Map.empty[c.Symbol, (c.TermName, c.Type, Option[c.Type])]
    }
    def apply[C <: blackbox.Context](c: C): Value[c.type] = new Value(c)
  }
}

class RemoteAccess[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq(
    Phase("remote:widen", widenRemoteNarrowing, after = Set("values:collect"), before = Set("impls:lift")),
    Phase("remote:marshalling", createMarshallables, after = Set("remote:widen"), before = Set("impls:lift")),
    Phase("remote:access", processRemoteAccesses, after = Set("remote:marshalling"), before = Set("impls:lift")))

  val commons = engine.require(Commons)
  val moduleInfo = engine.require(ModuleInfo)
  val peers = engine.require(Peers)
  val values = engine.require(Values)

  import engine._
  import engine.c.universe._
  import commons._
  import moduleInfo._
  import peers._
  import values._


  case class TransmittableInfo(
      base: Type, intermediate: Type, result: Type, proxy: Type, signature: Int) {
    def =:=(other: TransmittableInfo): Boolean  =
      other.base =:= base &&
      other.intermediate =:= intermediate &&
      other.result =:= result &&
      other.proxy =:= proxy &&
      other.signature == signature
  }

  object TransmittableInfo {
    def apply(fullyExpandedTree: Tree): TransmittableInfo = {
      val tpe = fullyExpandedTree.tpe
      TransmittableInfo(
        memberType(tpe, names.base),
        memberType(tpe, names.intermediate),
        memberType(tpe, names.result),
        memberType(tpe, names.proxy),
        (retyper untypecheck fullyExpandedTree).toString.hashCode)
    }

    implicit class IterableTransmittableInfoOps[T](
        iterable: Iterable[(TransmittableInfo, T)]) {
      def firstOfBaseType(tpe: Type): Option[(TransmittableInfo, T)] = iterable collectFirst {
        case value @ (info, _) if info.base =:= tpe => value
      }

      def firstOfArgumentType(tpe: Type): Option[(TransmittableInfo, T)] = iterable collectFirst {
        case value @ (info, _) if info.base =:= tpe && info.result =:= tpe => value
      }

      def firstWithInfo(info: TransmittableInfo): Option[T] = iterable collectFirst {
        case (otherInfo, value) if otherInfo =:= info => value
      }
    }
  }


  def widenRemoteNarrowing(records: List[Any]): List[Any] =
    records process {
      case record @ PlacedValue(_, _, _, _) =>
        object transformer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case q"$_[..$_](...$exprss)"
                if tree.nonEmpty &&
                   tree.symbol != null &&
                   tree.symbol.owner == symbols.Narrow =>
              transform(exprss.head.head)

            case _ =>
              super.transform(tree)
          }
        }

        record.copy(tree = transformer transform record.tree)
    }

  def createMarshallables(records: List[Any]): List[Any] = {
    val moduleName = uniqueName(module.symbol)
    val accessorGeneration =
      if (!module.symbol.isAbstract)
        AccessorGeneration.Required
      else
        AccessorGeneration.Preferred

    // all placed values and their corresponding type
    // (with placement information stripped from the type)
    val placedValues = (module.symbol.info.members.sorted flatMap { symbol =>
      val method = if (symbol.isMethod) Some(symbol.asMethod) else None

      method collect { case method if !method.isSetter =>
        decomposePlacementType(method.returnType, EmptyTree, method, method.pos, moduleDefinition = false) match {
          case Placed(peer, tpe, _, modality) if modality != Modality.Local =>
            if (method.typeParams.nonEmpty)
              c.abort(method.pos, "Placed methods cannot have type parameters")

            val types = List(tpe) :: (method.paramLists map { _ map { _.info }})
            val typeViews = types map { _ map { _.asSeenFrom(module.classSymbol) } }

            // if the view involves an abstract type member defined in the scope
            // of the `owner` symbol, we get some strange `ClassInfoType`,
            // which causes the call to the implicit resolution below
            // to run into an infinite loop (and also seems to be wrong in general)
            val invalidView = typeViews exists {
              _ exists {
                _ exists {
                  case ClassInfoType(_, _, _) => true
                  case _ => false
                }
              }
            }

            val List(res) :: argss = if (invalidView) types else typeViews

            def makeTuple(types: List[Type]) = types match {
              case List() => definitions.UnitTpe
              case List(tpe) => tpe
              case _ =>
                val size = types.size
                if (size > 22)
                  c.abort(method.pos, "More than 22 arguments are not supported")
                c.mirror.staticClass(s"_root_.scala.Tuple$size").toType mapArgs { _ => types }
            }

            val tuple = makeTuple(argss
              map { args => makeTuple(args filterNot { _ eq definitions.UnitTpe }) }
              filterNot { _ eq definitions.UnitTpe })

            modality match {
              case Modality.Subjective(subjective) =>
                Some((method, peer, res.typeArgs(1), Some(subjective), tuple))
              case _ =>
                Some((method, peer, res, None, tuple))
            }

          case _ =>
            None
        }
      }
    }).flatten

    // extract all implicitly constructed transmittables for all remote accesses
    val definedTransmittableTrees = records flatMap {
      case PlacedValue(_, tree, Some(peer), _) =>
        tree collect {
          case tree @ q"$_[..$_](...$exprss)" if tree.tpe real_<:< types.accessor =>
            val index = checkForTransmission(tree, peer)
            val q"$_[..$_](...$transmissionExprss)" = exprss(1)(index)

            if (transmissionExprss.isEmpty ||
                transmissionExprss.head.size < 4 ||
                transmissionExprss.head(3).tpe <:!< types.transmittable)
              c.abort(tree.pos, "Unexpected transmission value")

            transmissionExprss.head(3)
        }

      case _ =>
        List.empty
    }

    // retrieve information for all implicitly constructed transmittables
    // and make sure the transmittables for the same type are coherent
    val definedTransmittables = mutable.ListBuffer(definedTransmittableTrees.foldLeft(
        List.empty[(TransmittableInfo, Tree)]) { (transmittables, tree) =>
      val fullyExpandedTree = tree.fullyExpanded
      val info = TransmittableInfo(tree)
      val existing = transmittables firstWithInfo info

      existing foreach { existingTree =>
        if (!(fullyExpandedTree equalsStructure existingTree))
          c.abort(tree.pos, "Incoherent transmittables")
      }

      if (existing.isEmpty)
        info -> fullyExpandedTree :: transmittables
      else
        transmittables
    }: _*)

    // collect all inherited marshallables with their transmittable information
    val definedMarshallables = mutable.ListBuffer(module.symbol.info.members.sorted collect {
      case symbol @ MarshallableSymbol(info) => info -> symbol.asTerm.name
    }: _*)

    // collect symbols that are accessed remotely
    // so we need to construct marshallables for their arguments and result values
    val requiredTransmittables = (records flatMap {
      case PlacedValue(_, tree, _, _) =>
        tree collect {
          case tree @ q"$_[..$_](...$exprss)"
              if exprss.nonEmpty &&
                 exprss.head.nonEmpty &&
                 exprss.head.head.symbol != null &&
                 exprss.head.head.symbol.owner != symbols.Call &&
                 ((tree.tpe real_<:< types.accessor) ||
                  (tree.symbol != null &&
                   tree.symbol.owner == symbols.Call)) =>
            exprss.head.head.symbol
        }

      case _ =>
        List.empty
    }).toSet

    // keep a list of transmittables that could not be resolved to avoid redundant resolution attempts
    val unresolvedTransmittables = mutable.ListBuffer.empty[Type]

    // resolve or construct marshallables and placed value info instances
    var marshallableIndex = 0
    var placedValueIndex = 0
    var failed = false
    val (placedValueNames, dispatchValueClauses, accessorValues) = (placedValues map {
      case (symbol, peer, res, subjective, arg) =>
        val flags = Flag.SYNTHETIC | Flag.FINAL
        val signature = methodSignature(symbol, res)
        val hasArguments = arg =:!= definitions.UnitTpe
        val hasResult = res =:!= definitions.UnitTpe && res =:!= definitions.NothingTpe
        val definedArgTransmittable = definedTransmittables firstOfArgumentType arg
        val definedResTransmittable = definedTransmittables firstOfBaseType res
        val argTransmittableType = types.resolution mapArgs { args => List(arg, args(1), arg) ++ args.drop(3) }
        val resTransmittableType = types.resolution mapArgs { res :: _.tail }
        val transmittablesRequired = requiredTransmittables contains symbol

        // find an inherited placed value with the same signature
        // and whose transmittable type conforms to the transmittable used in this code
        val inheritedPlacedValue = (module.symbol.info.members.sorted collectFirst {
          case symbol @ PlacedValueSymbol(`signature`, argInfo, resInfo) =>
            val resConformant = definedResTransmittable forall { case (info, _) =>
              !hasResult && resInfo.isEmpty ||
               hasResult && resInfo.isDefined &&
               resInfo.get.result =:= info.result && resInfo.get.proxy =:= info.proxy
            }

            if ((!hasArguments && argInfo.isEmpty ||
                  hasArguments && argInfo.isDefined &&
                  argInfo.get.base =:= arg && argInfo.get.result =:= arg) &&
                (!hasResult && resInfo.isEmpty ||
                  hasResult && resInfo.isDefined &&
                  resInfo.get.base =:= res) &&
                resConformant)
              Some(Left(symbol.asTerm.name) -> Seq.empty)
            else
              None
        }).flatten

        // compute information of the transmittable used to access the value remotely
        // infer the transmittable first if necessary
        // skip this computation if accessor generation is deferred and the value is not accessed remotely
        def transmittable(definedTransmittable: Option[(TransmittableInfo, Tree)], transmittableType: Type) =
          if (!failed &&
              (accessorGeneration == AccessorGeneration.Forced ||
               inheritedPlacedValue.isEmpty &&
               (accessorGeneration != AccessorGeneration.Deferred ||
                definedTransmittable.nonEmpty ||
                transmittablesRequired)))
            definedTransmittable orElse {
              if (unresolvedTransmittables forall { _ =:!= transmittableType }) {
                val tree = c inferImplicitValue transmittableType match {
                  case q"$_[..$_]($expr)" if expr.tpe <:< types.transmittable =>
                    expr
                  case q"$_[..$_]($_[..$_]($expr))" if expr.tpe <:< types.transmittable =>
                    expr
                  case _ =>
                    EmptyTree
                }

                if (!tree.isEmpty) {
                  val fullyExpandedTree = tree.fullyExpanded
                  val transmittable = TransmittableInfo(fullyExpandedTree) -> fullyExpandedTree
                  definedTransmittables += transmittable
                  Some(transmittable)
                }
                else {
                  unresolvedTransmittables += transmittableType
                  None
                }
              }
              else
                None
            }
          else
            None

        val argTransmittable = transmittable(definedArgTransmittable, argTransmittableType)
        val resTransmittable = transmittable(definedResTransmittable, resTransmittableType)

        if (!failed &&
            (accessorGeneration == AccessorGeneration.Forced ||
             (inheritedPlacedValue.isEmpty &&
              (accessorGeneration == AccessorGeneration.Required ||
               accessorGeneration == AccessorGeneration.Preferred && resTransmittable.nonEmpty && argTransmittable.nonEmpty ||
               transmittablesRequired))))
          (argTransmittable, resTransmittable) match {
            // create marshallable and placed value info instances
            // for the successfully resolved transmittable
            // reusing inherited placed value info if possible
            case (Some((argInfo, argTree)), Some((resInfo, resTree))) =>
              val (name, seq) = inheritedPlacedValue getOrElse {
                def marshallable(info: TransmittableInfo, tree: Tree) =
                  // reuse existing marshallable if possible or
                  // create new marshallable if necessary
                  definedMarshallables firstWithInfo info map { name => q"$name" -> Seq.empty } getOrElse {
                    val marshallableName = TermName(s"$$loci$$mar$$$moduleName$$$marshallableIndex")
                    val transmittables = memberType(tree.tpe, names.transmittables)
                    val serializableType = types.serializable mapArgs { _ => List(info.intermediate) }
                    val resolutionTypes = List(info.base, info.intermediate, info.result)
                    val marshallableTypes = List(info.base, info.intermediate, info.result, info.proxy, transmittables)
                    val marshallableInfoType = types.marshallableInfo mapArgs { _ => List(info.intermediate) }
                    val annotation = q"new ${createTypeTree(marshallableInfoType, symbol.pos)}(${info.signature})"

                    def contextBuilders(tpe: Type): Tree = tpe.typeArgs match {
                      case Seq(tail, head) =>
                        q"${trees.list}(${contextBuilder(head)}, ${contextBuilders(tail)})"
                      case _ =>
                        q"${trees.delegate}(${contextBuilder(tpe)})"
                    }

                    def contextBuilder(tpe: Type): Tree = {
                      memberType(tpe, names.transmittables) match {
                        case tpe if tpe <:< types.delegates =>
                          q"${trees.delegating}(${contextBuilders(tpe.typeArgs.head)})"

                        case tpe if tpe <:< types.message =>
                          val transmittableType = tpe.typeArgs.head
                          val serializableType = types.serializable mapArgs { _ =>
                            List(memberType(transmittableType, names.intermediate))
                          }
                          q"""${trees.messaging}(
                            ${contextBuilder(transmittableType)},
                            ${trees.implicitly}[${createTypeTree(serializableType, symbol.pos)}])"""

                        case tpe if tpe <:< types.none =>
                          trees.none
                      }
                    }

                    marshallableIndex += 1
                    definedMarshallables += info -> marshallableName

                    q"$marshallableName" -> Seq(atPos(symbol.pos) {
                      q"""@$annotation $flags protected[this] val $marshallableName =
                        ${trees.marshallable}[..$marshallableTypes](
                          ${trees.resolution}[..$resolutionTypes]($tree),
                          ${trees.implicitly}[${createTypeTree(serializableType, symbol.pos)}],
                          ${contextBuilder(tree.tpe)})"""
                    })
                  }

                val nonStandardNonResult =
                  resInfo.proxy =:!= types.unitFuture &&
                  resInfo.proxy =:!= types.nothingFuture

                val (argName, argMarshallable) = if (hasArguments)
                  marshallable(argInfo, argTree) else q"null" -> Seq.empty
                val (resName, resMarshallable) = if (hasResult || nonStandardNonResult)
                  marshallable(resInfo, resTree) else q"null" -> Seq.empty

                Right(argName -> resName) -> (argMarshallable ++ resMarshallable)
              }

              name match {
                case Left(name) =>
                  // reusing inherited placed value info if possible
                  ((symbol, name, arg, subjective, true), None, seq)

                case Right((argName, resName)) =>
                  // create new placed value info if necessary
                  val placedValueName = TermName(s"$$loci$$val$$$moduleName$$$placedValueIndex")
                  val annotation = q"new ${types.placedRuntimeValueInfo}($signature, $argName, $resName)"
                  val placedValueType = types.placedRuntimeValue mapArgs { _ =>
                    List(argInfo.base, argInfo.result, argInfo.proxy, resInfo.base, resInfo.result, resInfo.proxy)
                  }

                  val placedValue = atPos(symbol.pos) {
                    q"""@$annotation $flags val $placedValueName =
                      new ${createTypeTree(placedValueType, symbol.pos)}(
                        ${trees.valueSignature}($signature, $$loci$$sig.path),
                        ${symbol.isStable},
                        $argName,
                        $resName)"""
                  }

                  // create dispatch clause
                  def arguments(paramLists: List[List[Symbol]], prefix: Tree) = {
                    def element(index: Int) = TermName(s"_$index")

                    def arguments(paramList: List[Symbol], prefix: Tree) = {
                      paramList.foldLeft(1 -> List.empty[Tree]) {
                        case ((index, args), symbol) =>
                          if (symbol.info =:= definitions.UnitTpe)
                            index -> (q"()" :: args)
                          else if ((paramList count { _.info =:!= definitions.UnitTpe }) == 1)
                            index + 1 -> (prefix :: args)
                          else
                            index + 1 -> (q"$prefix.${element(index)}" :: args)
                      }
                    }

                    val (_, argss) = paramLists.foldLeft(1 -> List.empty[List[Tree]]) {
                      case ((index, argss), args) =>
                        val (count, newargs) =
                          if ((paramLists count { _ exists { _.info =:!= definitions.UnitTpe } }) == 1)
                            arguments(args, prefix)
                          else
                            arguments(args, q"$prefix.${element(index)}")

                        if (count == 1)
                          index -> (newargs.reverse :: argss)
                        else
                          index + 1 -> (newargs.reverse :: argss)
                    }

                    argss.reverse
                  }

                  val invocation = (subjective
                    map { case subjective @ TypeRef(pre, sym, _) =>
                      (moduleStablePath(pre, q"${module.self}")
                        map { module =>
                          val name = TermName(s"$$loci$$peer$$sig$$${sym.name}")
                          val signature = q"$module.$name"

                          val ref = TermName("$loci$ref")
                          val remote = q"""$$loci$$abstraction.remote match {
                            case $ref: ${types.reference} if $ref.signature <= $signature =>
                              $ref
                            case _ =>
                              throw new ${types.illegalAccessException}("Illegal subjective access")
                          }"""

                          if (symbol.asTerm.isStable)
                            q"$$loci$$sys.subjectiveValue(${symbol.name}, $remote)"
                          else
                            q"${symbol.name}(...${arguments(symbol.paramLists, q"$$loci$$arguments")})($remote)"
                        }
                        getOrElse c.abort(symbol.pos,
                          s"Subjective definition may not refer to peer of another module: $subjective"))
                    } getOrElse {
                      q"${symbol.name}(...${arguments(symbol.paramLists, q"$$loci$$arguments")})"
                    })

                  val marshalling =
                    if (hasResult)
                      q"$resName.marshal($$loci$$response, $$loci$$abstraction)"
                    else
                      trees.empty

                  val resultMarshalling =
                    q"${trees.`try`} { $invocation } map { $$loci$$response => $marshalling }"

                  val requestProcessing = atPos(symbol.pos) {
                    if (hasArguments)
                      q"$argName.unmarshal($$loci$$request, $$loci$$abstraction) flatMap { $$loci$$arguments => $resultMarshalling }"
                    else
                      resultMarshalling
                  }

                  val dispatchClause =
                    Some(peer -> cq"$placedValueName.signature.name => $requestProcessing")

                  placedValueIndex += 1

                  ((symbol, placedValueName, arg, subjective, true), dispatchClause, seq :+ placedValue)
              }

            // create statement to implicitly resolve the transmittable whose resolution failed
            // to communicate the failure to the developer
            case (_, resTransmittable) =>
              failed = true

              val transmittableName = TermName(s"$$loci$$resolution$$failure$$$moduleName")
              val message = s"${if (resTransmittable.isEmpty) res else arg} is not transmittable"
              val tpe = if (resTransmittable.isEmpty) resTransmittableType else argTransmittableType
              val rhs = q"${trees.implicitly}[${createTypeTree(tpe, symbol.pos)}]"

              val pos = (module.tree.impl.parents
                collectFirst { case tree if tree.symbol == symbol.owner => tree.pos }
                getOrElse symbol.pos)

              ((symbol, termNames.EMPTY, arg, subjective, false), None, Seq(
                atPos(pos) {
                  q"@${types.compileTimeOnly}($message) $flags def $transmittableName(): ${definitions.UnitTpe} = $rhs"
                },
                atPos(pos) {
                  q"$transmittableName()"
                }))
          }
        else if (failed)
          ((symbol, termNames.EMPTY, arg, subjective, inheritedPlacedValue.nonEmpty), None, Seq.empty)
        else
          ((NoSymbol, termNames.EMPTY, NoType, None, inheritedPlacedValue.nonEmpty), None, Seq.empty)
    }).unzip3

    placedValueNames foreach { case (symbol, name, tpe, subjective, _) =>
      if (symbol != NoSymbol)
        PlacedValues.makeResolvable(symbol, name, tpe, subjective)
    }

    val modules = (records collect {
      case ModuleValue(symbol, _)
          if symbol.isTerm &&
             symbol.asTerm.isStable &&
             isMultitierModule(symbol.info, symbol.pos) =>
        symbol.asTerm
    }).distinct

    val dispatchModuleClauses = modules map { symbol =>
      cq"""${symbol.name.toString} => ${symbol.name}.$$loci$$dispatch(
        $$loci$$request,
        $$loci$$signature.copy($$loci$$signature.name, $$loci$$signature.path.tail),
        $$loci$$abstraction)"""
    }

    val moduleDispatch =
      if (dispatchModuleClauses.nonEmpty) {
        val tree = q"""${Flag.SYNTHETIC} def $$loci$$dispatch(
            $$loci$$request: ${types.messageBuffer},
            $$loci$$signature: ${types.signature},
            $$loci$$abstraction: ${types.abstractionRef}) =
          if ($$loci$$signature.path.isEmpty)
            super.$$loci$$dispatch($$loci$$request, $$loci$$signature, $$loci$$abstraction)
          else
            $$loci$$signature.path.head match {
              case ..$dispatchModuleClauses
              case _ => super.$$loci$$dispatch($$loci$$request, $$loci$$signature, $$loci$$abstraction)
            }"""

        Some(PlacedValueDef(NoSymbol, tree, None, Modality.None))
      }
      else
        None

    val valueDispatches = (dispatchValueClauses.flatten
      groupBy { case (peer, _) => peer }
      map { case (peer, clauses) =>
        val tree = q"""${Flag.SYNTHETIC} def $$loci$$dispatch(
            $$loci$$request: ${types.messageBuffer},
            $$loci$$signature: ${types.signature},
            $$loci$$abstraction: ${types.abstractionRef}) =
          if ($$loci$$signature.path.isEmpty)
            $$loci$$signature.name match {
              case ..${clauses map { case (_, clause) => clause } }
              case _ => super.$$loci$$dispatch($$loci$$request, $$loci$$signature, $$loci$$abstraction)
            }
          else
            super.$$loci$$dispatch($$loci$$request, $$loci$$signature, $$loci$$abstraction)"""

        PlacedValuePeerImpl(NoSymbol, tree, peer, Modality.None)
      })

    records ++ (accessorValues.flatten map { ModuleValue(NoSymbol, _) }) ++ moduleDispatch ++ valueDispatches
  }

  def processRemoteAccesses(records: List[Any]): List[Any] = {
    def arguments(paramLists: List[List[Symbol]], paramss: List[List[Tree]]) = {
      val arguments = (paramLists zip paramss).foldRight(List.empty[Tree]) {
        case ((paramList, params), tree) =>
          val args = (paramList zip params) collect {
            case (symbol, tree) if symbol.info =:!= definitions.UnitTpe => tree
          }

          args match {
            case List() => tree
            case List(arg) => arg :: tree
            case _ => q"(..$args)" :: tree
          }
      }

      arguments match {
        case List() => q"()"
        case List(arg) => arg
        case _ => q"(..$arguments)"
      }
    }

    def accessModuleValue(tree: Tree, name: TermName) = {
      object transformer extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case This(module.className) =>
            Ident(module.self)
          case tree: RefTree if tree.symbol == module.symbol =>
            Ident(module.self)
          case _ =>
            super.transform(tree)
        }
      }

      val Select(qualifier, _) = tree
      Select(transformer transform qualifier, name)
    }

    def accessPeerSignatureByTree(tree: Tree) =
      accessModuleValue(
        createTypeTree(tree),
        TermName(s"$$loci$$peer$$sig$$${tree.symbol.name}"))

    def accessPeerSignatureByType(tpe: Type, pos: Position) =
      accessModuleValue(
        createTypeTree(tpe, pos),
        TermName(s"$$loci$$peer$$sig$$${tpe.typeSymbol.name}"))

    def extractRemoteCall(tree: Tree) =
      if (tree.nonEmpty &&
          tree.symbol != null &&
          tree.symbol.owner == symbols.Call) {
        val q"$expr.$_[..$tpts](...$exprss)" = tree

        val (remotes, remotesType, signature) = {
          val q"$_[..$tpts](...$exprss)" = expr

          if (expr.symbol.owner == symbols.Select) {
            val remotes = exprss.head.foldRight[Tree](trees.nil) { (remote, tree) =>
              q"$tree.::(${trees.reference}($remote))"
            }
            (remotes, tpts.head.tpe, EmptyTree)
          }
          else if (tpts.nonEmpty)
            (trees.nil, tpts.head.tpe, accessPeerSignatureByTree(tpts.head))
          else
            (trees.nil, definitions.NothingTpe, EmptyTree)
        }

        Some((exprss.head.head, signature, remotes, remotesType))
      }
      else
        None

    def extractSelection(tree: Tree) =
      if (symbols.froms contains tree.symbol) {
        val q"$expr.$_[..$tpts](...$exprss)" = tree
        if (exprss.nonEmpty) {
          val remotes = exprss.head.foldRight[Tree](trees.nil) { (remote, tree) =>
            q"$tree.::(${trees.reference}($remote))"
          }
          (expr, EmptyTree, remotes, tpts.head.tpe)
        }
        else
          (expr, accessPeerSignatureByTree(tpts.head), trees.nil, tpts.head.tpe)
      }
      else
        (tree, EmptyTree, trees.nil, definitions.NothingTpe)

    def extractRemoteAccess(tree: Tree, peer: Symbol, remotesType: Type) = {
      def preventSuperAccess(tree: Tree): Unit = tree match {
        case q"$_.super[..$_].$_" =>
          c.abort(tree.pos, "Remote access to super value not allowed")
        case q"$expr.$_[..$_](...$_)" if tree.nonEmpty =>
          preventSuperAccess(expr)
        case _ =>
      }

      val q"$expr[..$_](...$exprss)" = tree

      preventSuperAccess(expr)

      val (placedValueName, argumentsType, subjective) =
        PlacedValues.resolve(tree.symbol, tree.pos)

      val peerType = tree.tpe.widen.typeArgs(1)

      val valueName =
        if (tree.symbol.isSynthetic && tree.symbol.isPrivate)
          "remote block"
        else
          tree.symbol.toString

      if (requirePeerType(peer).ties forall { _.tpe <:!< peerType })
        c.abort(tree.pos,
          s"No tie specified from ${peer.asType.toType} to $peerType")

      if (remotesType <:!< peerType)
        c.abort(tree.pos,
          s"$valueName placed on $peerType " +
          s"is accessed for $remotesType remote instances")

      subjective foreach { subjective =>
        if (internal.typeRef(internal.thisType(module.classSymbol), peer, List.empty) <:!< subjective)
          c.abort(tree.pos,
            s"$valueName subjectively dispatched to $subjective " +
            s"is accessed on ${peer.nameInEnclosing}")
      }

      val (placedValue, placedValuePeer) =
        if (placedValueName != termNames.EMPTY) {
          val placedValue = accessModuleValue(expr, placedValueName)
          val placedValuePeer = accessPeerSignatureByType(peerType, tree.pos)
          placedValue -> placedValuePeer
        }
        else
          q"null" -> q"null"

      (placedValue,
       placedValuePeer,
       arguments(tree.symbol.asMethod.paramLists, exprss),
       argumentsType)
    }

    records process {
      case record @ PlacedValue(_, _, Some(peer), _) =>
        object transformer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case tree: TypeTree if tree.original != null =>
              internal.setOriginal(tree, transform(tree.original))

            case tree: TypeTree if tree.tpe != null =>
              val tpe = tree.tpe map {
                case TypeRef(_, sym, _)
                    if sym == symbols.from ||
                       sym == symbols.fromSingle ||
                       sym == symbols.fromMultiple ||
                       sym == symbols.placedValue =>
                  definitions.UnitTpe
                case tpe =>
                  tpe
              }
              internal.setType(tree, tpe)

            case q"$expr[..$tpts](...$exprss)" if tree.tpe real_<:< types.accessor =>
              val index = checkForTransmission(tree, peer)

              val (value, signature, remotes, remotesType) =
                extractRemoteCall(exprss.head.head) getOrElse
                extractSelection(exprss.head.head)

              val (placedValue, placedValuePeer, arguments, argumentsType) =
                extractRemoteAccess(value, peer, remotesType)

              val exprs = exprss(1).updated(
                index,
                q"""new ${createTypeTree(types.remoteRequest.typeConstructor, value.pos)}(
                      $arguments, $placedValue, $placedValuePeer, $remotes, $$loci$$sys)""")

              atPos(value.pos) {
                transform(q"$expr[..${tpts map createTypeTree}](${trees.remoteValue})(..$exprs)")
              }

            case _ =>
              (extractRemoteCall(tree)
                map { case (value, signature, remotes, remotesType) =>
                  val (placedValue, placedValuePeer, arguments, _) =
                    extractRemoteAccess(value, peer, remotesType)

                  atPos(value.pos) {
                    transform(q"$$loci$$sys.invokeRemoteAccess($arguments, $placedValue, $placedValuePeer, $remotes, false)")
                  }
                }
                getOrElse {
                  if (symbols.froms contains tree.symbol)
                    c.abort(tree.pos, "Selection using `from` only allowed for remote access")

                  super.transform(tree)
                })
          }
        }

        record.copy(tree = transformer transform record.tree)
    }
  }

  private def checkForTransmission[T](tree: Tree, peer: Symbol): Int = {
    val q"$_[..$_](...$exprss)" = tree

    if (exprss.size != 2 || exprss.head.size != 1)
      c.abort(tree.pos, "Invalid remote accessor: " +
        "Implicit conversion with implicit argument list required")

    var index = 0
    var count = 0
    var result = -1

    val arg = exprss.head.head

    exprss(1) foreach { expr =>
      if (expr.tpe real_<:< types.transmission) {
        val q"$_[..$params](...$_)" = expr
        if (params.size < 10)
          c.abort(tree.pos, "Unexpected transmission value")

        val value = params(9).tpe

        val Seq(placed, _, to, _) =
          extractTag(expr.tpe, types.transmission, tree.pos).typeArgs

        val expectedValue =
          decomposePlacementType(arg.tpe.widen, EmptyTree, arg.symbol, arg.pos, moduleDefinition = false) match {
            case Placed(_, tpe, _, Modality.Subjective(_)) => Some(tpe.typeArgs(1))
            case Placed(_, tpe, _, _) => Some(tpe)
            case _ => None
          }

        if (to.typeSymbol != peer ||
            expectedValue.isEmpty ||
            expectedValue.get =:!= value ||
            arg.tpe <:!< placed)
          c.abort(tree.pos, "Invalid remote accessor: " +
            "Transmission value does not conform to remote access")

        result = index
        count += 1
      }

      index += 1
    }

    if (count != 1)
      c.abort(tree.pos, "Invalid remote accessor: " +
        "Exactly one transmission value required")

    result
  }

  private def methodSignature(symbol: MethodSymbol, returnType: Type) = {
    val name = symbol.name
    val args = (symbol.paramLists map {
      _ map { _.info.erasure.typeSymbol.fullName } mkString ("(", ",", ")")
    }).mkString
    val result = returnType.erasure.typeSymbol.fullName
    s"$name$args:$result"
  }

  private def memberType(tpe: Type, name: Name) = {
    val symbol = tpe member name
    symbol.info.asSeenFrom(tpe.underlying, symbol.owner)
  }

  object PlacedValues {
    def resolve(symbol: Symbol, pos: Position): (TermName, Type, Option[Type]) =
      cache.getOrElseUpdate(symbol, {
        val (resultType, peerType) =
          if (symbol.isMethod) {
            val method = symbol.asMethod

            decomposePlacementType(method.returnType, EmptyTree, method, method.pos, moduleDefinition = false) match {
              case Placed(_, tpe, _, modality) =>
                val resultType =
                  tpe.asSeenFrom(module.classSymbol) match {
                    case ClassInfoType(_, _, _) => tpe
                    case tpe => tpe
                  }

                modality match {
                  case Modality.Local =>
                    c.abort(pos, s"Local placed $symbol cannot be accessed remotely")
                  case Modality.None =>
                    resultType -> None
                  case Modality.Subjective(subjective) =>
                    resultType.typeArgs(1) -> Some(subjective)
                }

              case _ =>
                NoType -> None
            }
          }
          else
            NoType -> None

        if (resultType == NoType)
          c.abort(pos, s"$symbol is not placed")

        val signature = methodSignature(symbol.asMethod, resultType)

        (symbol.owner.info.members.sorted
          collectFirst {
            case symbol @ PlacedValueSymbol(`signature`, argInfo, _) =>
              (symbol.name.toTermName,
               argInfo map { _.base } getOrElse definitions.UnitTpe,
               peerType)
          }
          getOrElse c.abort(pos, s"Could not find remote accessor for placed $symbol"))
      })

    def makeResolvable(symbol: Symbol, name: TermName, tpe: Type, subjective: Option[Type]): Unit =
      cache += symbol -> ((name, tpe, subjective))

    private val cache: mutable.Map[Symbol, (TermName, Type, Option[Type])] =
      RemoteAccess.placedValueCache.get(c).cache
  }

  object MarshallableSymbol {
    def unapply(symbol: Symbol): Option[TransmittableInfo] =
      cache.getOrElse(symbol, {
        if (symbol.isMethod && (symbol.name.toString startsWith "$loci$mar$")) {
          val method = symbol.asMethod
          val tpe = method.info.finalResultType

          if (tpe <:< types.marshallable)
            method.allAnnotations map { _.tree } collectFirst {
              case tree @ Apply(_, List(Literal(Constant(signature: Int))))
                  if tree.tpe <:< types.marshallableInfo =>
                val Seq(base, result, proxy) = tpe.typeArgs
                val Seq(intermediate) = tree.tpe.typeArgs
                TransmittableInfo(base, intermediate, result, proxy, signature)
            }
          else
            None
        }
        else
          None
      })

    private val cache = mutable.Map.empty[Symbol, Option[TransmittableInfo]]
  }

  object PlacedValueSymbol {
    def unapply(symbol: Symbol): Option[(String, Option[TransmittableInfo], Option[TransmittableInfo])] =
      cache.getOrElse(symbol, {
        if (symbol.isMethod && (symbol.name.toString startsWith "$loci$val$")) {
          val method = symbol.asMethod
          val tpe = method.info.finalResultType

          if (tpe <:< types.placedRuntimeValue)
            (method.allAnnotations map { _.tree } collectFirst {
              case tree @ Apply(_, List(Literal(Constant(signature: String)), arg, res))
                  if tree.tpe <:< types.placedRuntimeValueInfo =>
                def info(tree: Tree) = (tree, tree.symbol) match {
                  case (Literal(Constant(null)), _) => Some(None)
                  case (_, MarshallableSymbol(info)) => Some(Some(info))
                  case _ => None
                }
                (info(arg), info(res)) match {
                  case (Some(argInfo), Some(resInfo)) =>
                    Some((signature, argInfo, resInfo))
                  case _ =>
                    None
                }
            }).flatten
          else
            None
        }
        else
          None
      })

    private val cache = mutable.Map.empty[Symbol, Option[(String, Option[TransmittableInfo], Option[TransmittableInfo])]]
  }
}
