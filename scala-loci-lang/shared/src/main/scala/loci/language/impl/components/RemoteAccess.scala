package loci
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
      val cache = mutable.Map.empty[(String, c.Symbol), (c.TermName, c.Type, Option[c.Type])]
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


  case class MarshallableInfo(base: Type, intermediate: Type, result: Type, proxy: Type, signature: Option[Int])

  object MarshallableInfo {
    def apply(info: TransmittableInfo, signature: Option[Int]): MarshallableInfo =
      MarshallableInfo(info.base, info.intermediate, info.result, info.proxy, signature)

    def apply(info: TransmittableInfo, fullyExpandedTree: Tree): MarshallableInfo =
      apply(info, Some((retyper untypecheck fullyExpandedTree).toString.hashCode))

    implicit class IterableMarshallableInfoOps[T](
        iterable: compatibility.IterableOnce[(MarshallableInfo, T)]) {
      def firstOfType(info: TransmittableInfo): Option[(MarshallableInfo, T)] =
        compatibility.iterable.collectFirst(iterable) {
          case value @ (otherInfo, _)
              if otherInfo.base =:= info.base &&
                 otherInfo.intermediate =:= info.intermediate &&
                 otherInfo.result =:= info.result &&
                 otherInfo.proxy =:= info.proxy =>
            value
        }

      def firstOf(info: MarshallableInfo): Option[T] =
        compatibility.iterable.collectFirst(iterable) {
          case (otherInfo, value)
              if otherInfo.base =:= info.base &&
                 otherInfo.intermediate =:= info.intermediate &&
                 otherInfo.result =:= info.result &&
                 otherInfo.proxy =:= info.proxy &&
                 otherInfo.signature == info.signature =>
            value
        }
    }
  }


  case class TransmittableInfo(base: Type, intermediate: Type, result: Type, proxy: Type, signature: Option[Int])

  object TransmittableInfo {
    def apply(info: MarshallableInfo, signature: Option[Int]): TransmittableInfo =
      TransmittableInfo(info.base, info.intermediate, info.result, info.proxy, signature)

    def apply(fullyExpandedTree: Tree): TransmittableInfo = {
      val tpe = fullyExpandedTree.tpe
      TransmittableInfo(
        memberType(tpe, names.base),
        memberType(tpe, names.intermediate),
        memberType(tpe, names.result),
        memberType(tpe, names.proxy),
        Some((retyper untypecheck fullyExpandedTree).toString.hashCode))
    }

    implicit class IterableTransmittableInfoOps[T](
        iterable: compatibility.IterableOnce[(TransmittableInfo, T)]) {
      def firstOfBaseType(tpe: Type): Option[(TransmittableInfo, T)] =
        compatibility.iterable.collectFirst(iterable) {
          case value @ (info, _) if info.base =:= tpe => value
        }

      def firstOfArgumentType(tpe: Type): Option[(TransmittableInfo, T)] =
        compatibility.iterable.collectFirst(iterable) {
          case value @ (info, _) if info.base =:= tpe && info.result =:= tpe => value
        }

      def firstOfAccessType(info: TransmittableInfo): Option[(TransmittableInfo, T)] =
        compatibility.iterable.collectFirst(iterable) {
          case value @ (otherInfo, _)
            if otherInfo.base =:= info.base &&
               otherInfo.result =:= info.result &&
               otherInfo.proxy =:= info.proxy =>
            value
        }
    }
  }


  def widenRemoteNarrowing(records: List[Any]): List[Any] = {
    var count = 0
    val result = records process {
      case record @ PlacedValue(_, _, _, _) =>
        object transformer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case q"$_[..$_](...$exprss)"
                if tree.nonEmpty &&
                   tree.symbol != null &&
                   tree.symbol.owner == symbols.Narrow =>
              count += 1
              transform(exprss.head.head)

            case _ =>
              super.transform(tree)
          }
        }

        record.copy(tree = transformer transform record.tree)
    }

    logging.debug(s" Eliminated $count syntactic ${if (count == 1) "from" else "froms"} for explicit remote value access")
    result
  }

  def createMarshallables(records: List[Any]): List[Any] = {
    val moduleName = uniqueName(module.symbol)
    val accessorGeneration =
      if (!module.symbol.isAbstract) {
        if ((module.accessorGeneration contains AccessorGeneration.Deferred) ||
            (module.accessorGeneration contains AccessorGeneration.Preferred))
          c.abort(c.enclosingPosition, "Accessor generation is required")

        module.accessorGeneration getOrElse AccessorGeneration.Required
      }
      else
        module.accessorGeneration getOrElse AccessorGeneration.Preferred

    // all placed values and their corresponding type
    // (with placement information stripped from the type)
    val placedValues = (module.classSymbol.selfType.members.sorted flatMap { symbol =>
      val method = if (symbol.isMethod) Some(symbol.asMethod) else None

      method collect { case method if !method.isSetter =>
        decomposePlacementType(method.returnType, EmptyTree, method, method.pos, moduleDefinition = false) match {
          case Placed(peer, tpe, _, modality) if modality != Modality.Local =>
            if (method.typeParams.nonEmpty)
              c.abort(method.pos, "Placed methods cannot have type parameters")

            def paramTypes(tpe: Type): List[List[Type]] = tpe match {
              case MethodType(params, resultType) => (params map { _.info }) :: paramTypes(resultType)
              case NullaryMethodType(tpe) => List.empty
              case _ => List.empty
            }

            def makeTuple(types: List[Type]) = types match {
              case List() => definitions.UnitTpe
              case List(tpe) => tpe
              case _ =>
                val size = types.size
                if (size > 22)
                  c.abort(method.pos, "More than 22 arguments are not supported")
                c.mirror.staticClass(s"_root_.scala.Tuple$size").toType mapArgs { _ => types }
            }

            val argss = paramTypes(method.info.asSeenFrom(module.classSymbol))

            val tuple = makeTuple(argss
              map { args => makeTuple(args filterNot { _ eq definitions.UnitTpe }) }
              filterNot { _ eq definitions.UnitTpe })

            modality match {
              case Modality.Subjective(subjective) =>
                Some((method, peer, tpe.typeArgs(1), Some(subjective), tuple))
              case _ =>
                Some((method, peer, tpe, None, tuple))
            }

          case _ =>
            None
        }
      }
    }).flatten

    // extract all implicitly constructed transmittables for all remote accesses
    val definedTransmittableTrees = records flatMap {
      case PlacedValue(_, tree, Some(peer), _) =>
        (tree collect {
          case tree @ q"$_[..$_](...$exprss)" if tree.tpe real_<:< types.remoteAccessor =>
            val index = checkForTransmission(tree, peer)
            val q"$_[..$_](...$transmissionExprss)" = exprss(1)(index): @unchecked
            transmissionExprss.headOption.toList flatMap {
              _ map extractTransmittable filter { _.nonEmpty }
            }
        }).flatten

      case _ =>
        List.empty
    }

    // retrieve information for all implicitly constructed transmittables
    // and make sure the transmittables for the same type are coherent
    val definedTransmittables = mutable.ListBuffer(definedTransmittableTrees.foldLeft(
        List.empty[(TransmittableInfo, Tree)]) { (transmittables, tree) =>
      val fullyExpandedTree = tree.fullyExpanded
      val info = TransmittableInfo(fullyExpandedTree)
      val existing = transmittables firstOfAccessType info

      existing foreach { case (existingInfo, _) =>
        if (info.signature != existingInfo.signature)
          c.abort(tree.pos, "Incoherent transmittables")
      }

      if (existing.isEmpty)
        info -> fullyExpandedTree :: transmittables
      else
        transmittables
    }: _*)

    // collect all inherited marshallables with their transmittable information
    val (declaredMarshallables, definedMarshallables) =
      mutable.ListBuffer(module.classSymbol.selfType.members.sorted collect {
        case symbol @ MarshallableSymbol(info, name) =>
          info -> (symbol -> name)
      }: _*) partition { case (info, _) => info.signature.isEmpty }

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
                 ((tree.tpe real_<:< types.remoteAccessor) ||
                  (tree.symbol != null &&
                   tree.symbol.owner == symbols.Call)) =>
            exprss.head.head.symbol
        }

      case _ =>
        List.empty
    }).toSet

    // resolve transmittables, either using the already resolved trees or by invoking implicit resolution
    // generate an error if transmittable resolution is required but not possible
    val resolvedSerializables = mutable.ListBuffer.empty[(Type, Either[Tree, Tree])]
    val unresolvedTransmittables = mutable.ListBuffer.empty[Type]
    var failed = false

    def resolveSerializable(tpe: Type, pos: Position) =
      (resolvedSerializables
        collectFirst { case (serializableType, serializable) if serializableType =:= tpe =>
          serializable
        }
        getOrElse {
          val tree = typecheckInPositionContext(q"${trees.serializable}[$tpe]", pos) match {
            case q"$_[$_]($expr)" => expr
            case _ => EmptyTree
          }

          tree foreach { internal.setPos(_, NoPosition) }

          val dummyTree = tree exists {
            case q"$expr[$_](...$_)" => expr.symbol.owner == symbols.serializable
            case _ => false
          }

          val serializable =
            if (tree.isEmpty) {
              val resolutionName = TermName(s"$$loci$$resolution$$failure$$res")
              val errorName = TermName(s"$$loci$$resolution$$failure$$err")

              val message = s"$tpe is not serializable"
              val rhs = q"${trees.implicitly}[${createTypeTree(types.serializable mapArgs { _ => List(tpe) }, pos)}]"

              rhs foreach { internal.setPos(_, NoPosition) }

              Left(atPos(pos) {
                q"""${Flag.SYNTHETIC} def $resolutionName() = $rhs
                    @${types.compileTimeOnly}($message) ${Flag.SYNTHETIC} def $errorName(): ${definitions.UnitTpe} = ()
                    $errorName()
                    $resolutionName()""" })
            }
            else if (dummyTree)
              Left(tree)
            else
              Right(tree)

          resolvedSerializables += tpe -> serializable

          serializable
        })

    def resolveTransmittables(
        transmittables: Seq[(Option[(TransmittableInfo, Tree)], Type)],
        pos: Position,
        inheritedPlacedValue: Boolean,
        transmittablesRequired: Boolean) = {

      // compute information of the transmittable used to access the value remotely
      // infer the transmittable first if necessary
      // skip this computation if accessor generation is deferred and the value is not accessed remotely
      def transmittable(definedTransmittable: Option[(TransmittableInfo, Tree)], transmittableType: Type) =
        if (!failed &&
            (accessorGeneration == AccessorGeneration.Forced ||
             !inheritedPlacedValue &&
             (accessorGeneration != AccessorGeneration.Deferred ||
              definedTransmittable.nonEmpty ||
              transmittablesRequired))) {
          val resolvedTransmittable = definedTransmittable orElse {
            if (unresolvedTransmittables forall { _ =:!= transmittableType }) {
              val tree = extractTransmittable(typecheckInPositionContext(q"${trees.implicitly}[$transmittableType]", pos))

              tree foreach { internal.setPos(_, NoPosition) }

              val fullyExpandedTree = tree.fullyExpanded
              Some(TransmittableInfo(fullyExpandedTree) -> fullyExpandedTree)
            }
            else
              None
          }

          val transmittable =
            compatibility.either.flatMap(resolvedTransmittable.toRight(Option.empty[Tree])) {
              case resolvedTransmittable @ (_, tree) =>
                val dummyTransmittableTree = DummyTransmittable(tree)

                if (tree.nonEmpty &&
                    dummyTransmittableTree != DummyTransmittable.NonParameter &&
                    (dummyTransmittableTree == DummyTransmittable.None ||
                     accessorGeneration != AccessorGeneration.Required &&
                     accessorGeneration != AccessorGeneration.Forced))
                  Right(resolvedTransmittable)
                else
                  Left(Some(tree))
            }

          if (transmittable.isRight)
            definedTransmittables += compatibility.either.right(transmittable)
          else
            unresolvedTransmittables += transmittableType

          transmittable
        }
        else
          Left(None)

      val resolvedTransmittables = transmittables map (transmittable _).tupled

      if (!failed &&
          (accessorGeneration == AccessorGeneration.Forced ||
           (!inheritedPlacedValue &&
            (accessorGeneration == AccessorGeneration.Required ||
             accessorGeneration == AccessorGeneration.Preferred && (resolvedTransmittables forall { _.isRight }) ||
             transmittablesRequired)))) {
        if (resolvedTransmittables exists { _.isLeft }) {
          // create statement to implicitly resolve the transmittable whose resolution failed
          // to communicate the failure to the developer
          failed = true

          val (tpe, tree) = ((transmittables zip resolvedTransmittables).reverse collectFirst {
            case ((_, tpe), Left(tree)) => tpe -> tree
          }).get

          val resolutionName = TermName(s"$$loci$$resolution$$failure$$res")
          val errorName = TermName(s"$$loci$$resolution$$failure$$err")

          val message = s"${tpe.typeArgs.head} is not transmittable"
          val rhs = tree getOrElse q"${trees.implicitly}[${createTypeTree(tpe, pos)}]"

          rhs foreach { internal.setPos(_, NoPosition) }

          Left(
            Seq(
              q"${Flag.SYNTHETIC} def $resolutionName(): ${definitions.UnitTpe} = $rhs",
              q"$resolutionName()",
              q"@${types.compileTimeOnly}($message) ${Flag.SYNTHETIC} def $errorName(): ${definitions.UnitTpe} = ()",
              q"$errorName()")
            map atPos(pos))
        }
        else
          Right(resolvedTransmittables map compatibility.either.right)
      }
      else
        Right(Seq.empty)
    }

    // construct marshallable instances reusing existing marshallables if possible
    val implementedMarshallables = mutable.Set.empty[TermName]
    var marshallableIndex = 0

    def createMarshallable(info: TransmittableInfo, tree: Tree, pos: Position, implementationRequired: Boolean) = {
      val implementation =
        if (implementationRequired)
          MarshallableImplementation.Required
        else
          MarshallableImplementation.Deferrable

      val (name, marshallable) = generateMarshallable(
        info, tree, pos, TermName(s"$$loci$$mar$$$moduleName$$$marshallableIndex"), implementation)

      if (marshallable.nonEmpty)
        marshallableIndex += 1

      val nameTree = name map { name => q"$name" }
      (nameTree, nameTree, marshallable)
    }

    def implementMarshallable(info: TransmittableInfo, tree: Tree, pos: Position, name: TermName) = {
      val (_, marshallable) = generateMarshallable(info, tree, pos, name, MarshallableImplementation.Omissible)
      marshallable
    }

    object MarshallableImplementation extends Enumeration {
      val Deferrable, Required, Omissible = Value
    }

    def generateMarshallable(
        info: TransmittableInfo,
        tree: Tree,
        pos: Position,
        name: TermName,
        implementation: MarshallableImplementation.Value) = {
      val dummyTransmittableTree = DummyTransmittable(tree) != DummyTransmittable.None
      val marshallables =
        if (dummyTransmittableTree && implementation != MarshallableImplementation.Omissible)
          definedMarshallables.iterator ++ declaredMarshallables.iterator
        else
          definedMarshallables.iterator

      val marshallableInfoType = types.marshallableInfo mapArgs { _ => List(info.intermediate) }
      val marshallableInfoTree = createTypeTree(marshallableInfoType, pos)
      val marshallableMods = Modifiers(Flag.SYNTHETIC | Flag.PROTECTED | Flag.LOCAL | Flag.LAZY)

      val abstractValueAnnotation = q"new ${types.abstractValue}"

      def marshallableImplementation(marshallableName: TermName, expr: Tree, exprInfo: MarshallableInfo, isDeferred: Boolean) =
        if (!(implementedMarshallables contains marshallableName)) {
          if (isDeferred)
            logging.debug(s"  Implementing deferred marshallable for ${info.base} ~> ${info.proxy}")
          else
            logging.debug(s"  Creating marshallable for ${info.base} ~> ${info.proxy}")

          implementedMarshallables += marshallableName
          definedMarshallables += exprInfo -> (NoSymbol -> marshallableName)

          val annotation = q"new $marshallableInfoTree(${exprInfo.signature getOrElse 0})"
          val mods =
            if (exprInfo.signature.nonEmpty)
              marshallableMods mapAnnotations { _ => List(annotation) }
            else
              marshallableMods mapAnnotations { _ => List(abstractValueAnnotation, annotation) }

          Some(atPos(pos) { q"$mods val $marshallableName = $expr" })
        }
        else
          None

      def reusedMarshallableImplementation(info: MarshallableInfo, marshallableName: TermName) =
        if (implementation != MarshallableImplementation.Omissible) {
          logging.debug("  " +
            s"Reusing${if (info.signature.isEmpty) " deferred " else " "}marshallable " +
            s"for ${info.base} ~> ${info.proxy}")
          Some(marshallableName) -> None
        }
        else
          None -> marshallableImplementation(name, q"$marshallableName", info, isDeferred = true)

      // reuse existing marshallable if possible or
      // create new marshallable if necessary
      val reusedMarshallable =
        if (accessorGeneration != AccessorGeneration.Forced)
          (marshallables
            firstOfType info
            map { case (info, (_, marshallableName)) =>
              reusedMarshallableImplementation(info, marshallableName)
            })
        else
          None

      reusedMarshallable getOrElse {
        if (!dummyTransmittableTree) {
          val transmittables = memberType(tree.tpe, names.transmittables)
          val transmittableTypes = List(info.base, info.intermediate, info.result, info.proxy, transmittables)
          val resolutionType = types.resolution mapArgs { _ => transmittableTypes }

          val serializable = resolveSerializable(info.intermediate, pos)
          var serializableResolutionFailure = serializable.isLeft

          def contextBuilders(tpe: Type): Tree = tpe.typeArgs match {
            case Seq(tail, head) =>
              q"${trees.list}(${contextBuilder(head)}, ${contextBuilders(tail)})"
            case _ =>
              q"${trees.delegate}(${contextBuilder(tpe)})"
          }

          def contextBuilder(tpe: Type): Tree = {
            (memberType(tpe, names.transmittables): @unchecked) match {
              case tpe if tpe <:< types.delegates =>
                q"${trees.delegating}(${contextBuilders(tpe.typeArgs.head)})"

              case tpe if tpe <:< types.message =>
                val transmittableType = tpe.typeArgs.head
                val serializable = resolveSerializable(memberType(transmittableType, names.intermediate), pos)

                if (serializable.isLeft)
                  serializableResolutionFailure = true

                q"""${trees.messaging}(
                  ${contextBuilder(transmittableType)},
                  ${serializable.merge})"""

              case tpe if tpe <:< types.none =>
                trees.none
            }
          }

          val expr = q"""${trees.marshallable}[..$transmittableTypes](
            new $resolutionType($tree),
            ${serializable.merge},
            ${contextBuilder(tree.tpe)})"""

          val exprInfo = MarshallableInfo(info, expr.fullyExpanded)

          (definedMarshallables
            firstOf exprInfo
            map { case (_, marshallableName) =>
              reusedMarshallableImplementation(exprInfo, marshallableName)
            }
            getOrElse {
              if (serializableResolutionFailure && implementation != MarshallableImplementation.Required)
                None -> None
              else if (implementation == MarshallableImplementation.Omissible)
                None -> marshallableImplementation(name, expr, exprInfo, isDeferred = true)
              else
                Some(name) -> marshallableImplementation(name, expr, exprInfo, isDeferred = false)
            })
        }
        else if (implementation != MarshallableImplementation.Omissible) {
          logging.debug(s"  Deferring marshallable for ${info.base} ~> ${info.proxy}")

          declaredMarshallables += MarshallableInfo(info, None) -> (NoSymbol -> name)

          val marshallableType = types.marshallable mapArgs { _ => List(info.base, info.result, info.proxy) }
          val annotation = q"new $marshallableInfoTree(0)"
          val mods = marshallableMods mapAnnotations { _ => List(abstractValueAnnotation, annotation) }

          Some(name) -> Some(atPos(pos) { q"$mods val $name: $marshallableType = null" })
        }
        else
          None -> None
      }
    }

    // resolve or construct marshallables and placed value info instances
    var placedValueIndex = 0

    val (placedValueNames, dispatchValueClauses, accessorValues) = (placedValues map {
      case (symbol, peer, res, subjective, arg) =>
        val ancestors = symbol.ancestors
        val signature = methodSignature(symbol, res)
        val hasArguments = arg =:!= definitions.UnitTpe
        val hasResult = res =:!= definitions.UnitTpe && res =:!= definitions.NothingTpe
        val definedArgTransmittable = definedTransmittables firstOfArgumentType arg
        val definedResTransmittable = definedTransmittables firstOfBaseType res
        val argTransmittableType = types.resolution mapArgs { args => List(arg, args(1), arg) ++ args.drop(3) }
        val resTransmittableType = types.resolution mapArgs { res :: _.tail }
        val transmittablesRequired = requiredTransmittables contains symbol

        logging.debug(s" Processing${if (symbol.owner != module.classSymbol) " inherited " else " "}placed value $signature")

        val pos = (module.tree.impl.parents
          collectFirst { case tree if ancestors contains tree.symbol => tree.pos }
          getOrElse symbol.pos)

        // find an inherited inheritedPlacedValueplaced value with the same signature
        // and whose transmittable type conforms to the transmittable used in this code
        val inheritedPlacedValue = (module.classSymbol.selfType.members.sorted
          collectFirst {
            case symbol @ PlacedValueSymbol(`signature`, argInfo, resInfo) =>
              val resConformant = definedResTransmittable forall { case (info, _) =>
                !hasResult && resInfo.isEmpty ||
                 hasResult && resInfo.isDefined &&
                 resInfo.get.result =:= info.result && resInfo.get.proxy =:= info.proxy
              }

              (!hasArguments && argInfo.isEmpty ||
                hasArguments && argInfo.isDefined &&
                argInfo.get.base =:= arg && argInfo.get.result =:= arg) &&
              (!hasResult && resInfo.isEmpty ||
                hasResult && resInfo.isDefined &&
                resInfo.get.base =:= res) &&
              resConformant
          }
          contains true)

        (resolveTransmittables(
            Seq(definedArgTransmittable -> argTransmittableType, definedResTransmittable -> resTransmittableType),
            pos,
            inheritedPlacedValue,
            transmittablesRequired): @unchecked) match {
          case Left(trees) =>
            ((symbol, termNames.EMPTY, arg, subjective, false), None, trees)

          case Right(Seq()) if failed =>
            ((symbol, termNames.EMPTY, arg, subjective, inheritedPlacedValue), None, Seq.empty)

          case Right(Seq()) =>
            ((NoSymbol, termNames.EMPTY, NoType, None, inheritedPlacedValue), None, Seq.empty)

          case Right(Seq((argInfo, argTree), (resInfo, resTree))) =>
            // create marshallable and placed value info instances
            // for the successfully resolved transmittable
            val marshallables = {
              val nonStandardNonResult =
                resInfo.proxy =:!= types.unitFuture &&
                resInfo.proxy =:!= types.nothingFuture

              val implementationRequired =
                accessorGeneration == AccessorGeneration.Forced ||
                accessorGeneration == AccessorGeneration.Required ||
                transmittablesRequired

              val (argValue, argAnnotation, argMarshallable) =
                if (hasArguments)
                  createMarshallable(argInfo, argTree, pos, implementationRequired)
                else
                  (Some(trees.unitMarshallable), Some(q"null"), None)

              val (resValue, resAnnotation, resMarshallable) =
                if (hasResult || nonStandardNonResult)
                  createMarshallable(resInfo, resTree, pos, implementationRequired)
                else if (res =:= definitions.UnitTpe)
                  (Some(trees.unitMarshallable), Some(q"null"), None)
                else
                  (Some(trees.nothingMarshallable), Some(q"null"), None)

              if (argValue.nonEmpty && argAnnotation.nonEmpty && resValue.nonEmpty && resAnnotation.nonEmpty)
                Some((argValue.get, argAnnotation.get, resValue.get, resAnnotation.get, (argMarshallable ++ resMarshallable).toSeq))
              else
                None
            }

            (marshallables
              map { case (argValue, argAnnotation, resValue, resAnnotation, marshallables) =>
                // create new placed value info if necessary
                val placedValueName = TermName(s"$$loci$$val$$$moduleName$$$placedValueIndex")
                val annotation = q"new ${types.placedRuntimeValueInfo}($signature, $argAnnotation, $resAnnotation)"
                val placedValueType = types.placedRuntimeValue mapArgs { _ =>
                  List(argInfo.base, argInfo.result, resInfo.base, resInfo.proxy)
                }

                val placedValue = atPos(pos) {
                  q"""@$annotation ${Flag.SYNTHETIC} final val $placedValueName =
                    new ${createTypeTree(placedValueType, pos)}(
                      ${trees.valueSignature}($signature, $$loci$$mod, $$loci$$sig.path),
                      ${symbol.isStable},
                      $argValue,
                      $resValue)"""
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
                  map { subjective =>
                    val TypeRef(pre, sym, _) = subjective: @unchecked
                    (moduleStablePath(pre, q"${module.self}")
                      map { module =>
                        val name = TermName(s"$$loci$$peer$$sig$$${sym.name}")
                        val signature = q"$module.$name"

                        val remote = q"""
                          if ($$loci$$reference.remote.signature <= $signature)
                            $$loci$$reference.remote
                          else
                            throw new ${types.remoteAccessException}("Illegal subjective access")"""

                        if (symbol.asTerm.isStable)
                          q"$$loci$$sys.subjectiveValue(${symbol.name}, $remote)"
                        else
                          q"${symbol.name}(...${arguments(symbol.paramLists, q"$$loci$$arguments")})($remote)"
                      }
                      getOrElse c.abort(pos,
                        s"Subjective definition may not refer to peer of another module: $subjective"))
                  } getOrElse {
                    q"${symbol.name}(...${arguments(symbol.paramLists, q"$$loci$$arguments")})"
                  })

                val marshalling =
                  if (hasResult)
                    q"$placedValueName.result.marshal($$loci$$response, $$loci$$reference)"
                  else
                    trees.empty

                val resultMarshalling =
                  q"${trees.`try`} { $invocation } map { $$loci$$response => $marshalling }"

                val requestProcessing = atPos(pos) {
                  if (hasArguments)
                    q"$placedValueName.arguments.unmarshal($$loci$$request, $$loci$$reference) flatMap { $$loci$$arguments => $resultMarshalling }"
                  else
                    resultMarshalling
                }

                val dispatchClause =
                  Some(peer -> cq"$placedValueName.signature.name => $requestProcessing")

                placedValueIndex += 1

                ((symbol, placedValueName, arg, subjective, true), dispatchClause, marshallables :+ placedValue)
            }
            getOrElse {
              ((NoSymbol, termNames.EMPTY, NoType, None, inheritedPlacedValue), None, Seq.empty)
            })
        }
    }).unzip3

    // resolve or construct marshallables whose instantiation was deferred to sub modules
    logging.debug(" Processing deferred marshallables of super modules")
    val marshallableValues = declaredMarshallables.toList flatMap {
      case (_, (NoSymbol, _)) =>
        Seq.empty

      case (info, (symbol, marshallableName)) =>
        val ancestors = symbol.ancestors
        val definedTransmittable = definedTransmittables firstOfAccessType TransmittableInfo(info, None)
        val transmittableType = types.resolution mapArgs { args => List(info.base, args(1), info.base, info.proxy) :+ args.last }

        val pos = (module.tree.impl.parents
          collectFirst { case tree if ancestors contains tree.symbol => tree.pos }
          getOrElse symbol.pos)

        (resolveTransmittables(
            Seq(definedTransmittable -> transmittableType),
            pos,
            inheritedPlacedValue = false,
            transmittablesRequired = false): @unchecked) match {
          case Left(trees) =>
            trees
          case Right(Seq()) =>
            Seq.empty
          case Right(Seq((info, tree))) =>
            implementMarshallable(info, tree, pos, marshallableName)
        }
    }

    val path = module.path mkString "."

    placedValueNames foreach { case (symbol, name, tpe, subjective, _) =>
      if (symbol != NoSymbol)
        PlacedValues.makeResolvable(path, symbol, name, tpe, subjective)
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
        $$loci$$signature.copy($$loci$$signature.name, $$loci$$signature.module, $$loci$$signature.path.tail),
        $$loci$$reference)"""
    }

    val moduleDispatch =
      if (dispatchModuleClauses.nonEmpty) {
        val tree = q"""${Flag.SYNTHETIC} def $$loci$$dispatch(
            $$loci$$request: ${types.messageBuffer},
            $$loci$$signature: ${types.signature},
            $$loci$$reference: ${types.valueReference}) =
          if ($$loci$$signature.path.isEmpty)
            super.$$loci$$dispatch($$loci$$request, $$loci$$signature, $$loci$$reference)
          else
            $$loci$$signature.path.head match {
              case ..$dispatchModuleClauses
              case _ => super.$$loci$$dispatch($$loci$$request, $$loci$$signature, $$loci$$reference)
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
            $$loci$$reference: ${types.valueReference}) =
          if ($$loci$$signature.path.isEmpty) {
            if ($$loci$$signature.module == $$loci$$mod)
              $$loci$$signature.name match {
                case ..${clauses map { case (_, clause) => clause } }
                case _ => super.$$loci$$dispatch($$loci$$request, $$loci$$signature, $$loci$$reference)
              }
            else
              super.$$loci$$dispatch($$loci$$request, $$loci$$signature, $$loci$$reference)
          }
          else
            super.$$loci$$dispatch($$loci$$request, $$loci$$signature, $$loci$$reference)"""

        PlacedValuePeerImpl(NoSymbol, tree, peer, Modality.None)
      })

    records ++
    (accessorValues.flatten ++ marshallableValues map { ModuleValue(NoSymbol, _) }) ++
    moduleDispatch ++
    valueDispatches
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

      tree match {
        case Select(qualifier, _) =>
          Select(transformer transform qualifier, name)
        case _ =>
          Ident(name)
      }
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
        val q"$expr.$_[..$tpts](...$exprss)" = tree: @unchecked

        val (remotes, instanceBased, remotesType, signature) = {
          val q"$_[..$tpts](...$exprss)" = expr: @unchecked

          if (expr.symbol.owner == symbols.Select) {
            val dynamicRemoteSequence =
              exprss.head.size == 1 && exprss.head.head.tpe <:< types.remoteSeq

            val remotes =
              if (dynamicRemoteSequence)
                q"${exprss.head.head} map ${trees.reference}"
              else
                exprss.head.foldRight[Tree](trees.nil) { (remote, result) =>
                  q"$result.::(${trees.reference}($remote))"
                }

            (remotes, dynamicRemoteSequence, tpts.head.tpe, EmptyTree)
          }
          else if (tpts.nonEmpty)
            (trees.nil, false, tpts.head.tpe, accessPeerSignatureByTree(tpts.head))
          else
            (trees.nil, false, definitions.NothingTpe, EmptyTree)
        }

        Some((exprss.head.head, signature, remotes, instanceBased, remotesType))
      }
      else
        None

    def extractSelection(tree: Tree) =
      if (symbols.froms contains tree.symbol) {
        val (expr, tpts, exprss) = (tree: @unchecked) match {
          case q"$expr.$_[..$tpts]($selection(new $tuple(...$exprss)))"
            if selection.symbol.owner == symbols.remoteSelection &&
               (tuple.symbol.fullName startsWith names.tuple) =>
            (expr, tpts, exprss)
          case q"$expr.$_[..$tpts]($selection($tuple(...$exprss)))"
            if selection.symbol.owner == symbols.remoteSelection &&
               (tuple.symbol.owner.fullName startsWith names.tuple) =>
            (expr, tpts, exprss)
          case q"$expr.$_[..$tpts](...$exprss)" =>
            (expr, tpts, exprss)
        }

        if (exprss.nonEmpty) {
          val dynamicRemoteSequence =
            exprss.head.size == 1 && exprss.head.head.tpe <:< types.remoteSeq

          val remotes =
            if (dynamicRemoteSequence)
              q"${exprss.head.head} map ${trees.reference}"
            else
              exprss.head.foldRight[Tree](trees.nil) { (remote, result) =>
                if (remote.tpe <:!< types.remote)
                  c.abort(tree.pos, "Unexpected selection")
                q"$result.::(${trees.reference}($remote))"
              }

          (expr, EmptyTree, remotes, dynamicRemoteSequence, tpts.head.tpe)
        }
        else
          (expr, accessPeerSignatureByTree(tpts.head), trees.nil, false, tpts.head.tpe)
      }
      else
        (tree, EmptyTree, trees.nil, false, definitions.NothingTpe)

    def extractRemoteAccess(tree: Tree, peer: Symbol, remotesType: Type) = {
      def preventSuperAccess(tree: Tree): Unit = tree match {
        case q"$_.super[..$_].$_" =>
          c.abort(tree.pos, "Remote access to super value not allowed")
        case q"$expr.$_[..$_](...$_)" if tree.nonEmpty =>
          preventSuperAccess(expr)
        case _ =>
      }

      val q"$expr[..$_](...$exprss)" = tree: @unchecked

      preventSuperAccess(expr)

      val path = expr match {
        case Select(qualifier, _) =>
          def path(tree: Tree): List[String] = tree match {
            case Select(Ident(termNames.EMPTY), name) =>
              List(name.toString)
            case Select(qualifier, name) =>
              path(qualifier) :+ name.toString
            case _ =>
              List.empty
          }

          module.path ++ (moduleStablePath(qualifier.tpe, Ident(termNames.EMPTY)).toList flatMap path) mkString "."

        case _ =>
          ""
      }

      val (placedValueName, _, subjective) =
        PlacedValues.resolve(path, tree.symbol, tree.pos)

      val peerType = tree.tpe.finalResultType.widen.asSeenFrom(module.classSymbol).typeArgs(1)

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
       arguments(tree.symbol.asMethod.paramLists, exprss))
    }

    var count = 0
    val result = records process {
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

            case q"$expr[..$tpts](...$exprss)" if tree.tpe real_<:< types.remoteAccessor =>
              count += 1

              val index = checkForTransmission(tree, peer)

              val (value, _, remotes, dynamicRemoteSequence, remotesType) =
                extractRemoteCall(exprss.head.head) getOrElse
                extractSelection(exprss.head.head)

              val (placedValue, placedValuePeer, arguments) =
                extractRemoteAccess(value, peer, remotesType)

              val instances = c.freshName(TermName("instances"))
              val exprs = exprss(1).updated(
                index,
                q"""val $instances = $remotes
                    new ${createTypeTree(types.remoteRequest.typeConstructor, value.pos)}(
                      $arguments, $placedValue, $placedValuePeer, $instances, $dynamicRemoteSequence, $$loci$$sys)""")

              atPos(value.pos) {
                transform(q"$expr[..${tpts map createTypeTree}](${trees.remoteValue})(..$exprs)")
              }

            case _ =>
              (extractRemoteCall(tree)
                map { case (value, _, remotes, dynamicRemoteSequence, remotesType) =>
                  count += 1

                  val (placedValue, placedValuePeer, arguments) =
                    extractRemoteAccess(value, peer, remotesType)

                  atPos(value.pos) {
                    val instances = c.freshName(TermName("instances"))
                    val invokeRemoteAccess =
                      q"$$loci$$sys.invokeRemoteAccess($arguments, $placedValue, $placedValuePeer, $instances, false)"

                    if (dynamicRemoteSequence)
                      transform(q"val $instances = $remotes; if($instances.nonEmpty) $invokeRemoteAccess")
                    else
                      transform(q"val $instances = $remotes; $invokeRemoteAccess")
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

    logging.debug(s" Processed $count remote ${if (count == 1) "access" else "accesses"}")

    result
  }

  private def checkForTransmission(tree: Tree, peer: Symbol): Int = {
    val q"$_[..$_](...$exprss)" = tree: @unchecked

    if (exprss.size != 2 || exprss.head.size != 1)
      c.abort(tree.pos, "Invalid remote accessor: " +
        "Implicit conversion with implicit argument list required")

    var index = 0
    var count = 0
    var result = -1

    val arg = exprss.head.head
    val tpe = arg.tpe.finalResultType.widen.asSeenFrom(module.classSymbol)

    exprss(1) foreach { expr =>
      if (expr.tpe real_<:< types.transmission) {
        val q"$_[..$params](...$_)" = expr: @unchecked

        val value =
          if (params.size < 11)
            definitions.NothingTpe
          else
            params(4).tpe

        val Seq(placedValue, placedPeer, _, to, _) =
          extractTag(expr.tpe, types.transmission, tree.pos).typeArgs: @unchecked

        val expectedValue =
          decomposePlacementType(tpe, EmptyTree, arg.symbol, arg.pos, moduleDefinition = false) match {
            case Placed(_, tpe, _, Modality.Subjective(_)) => Some(tpe.typeArgs(1))
            case Placed(_, tpe, _, _) => Some(tpe)
            case _ => None
          }

        if (to.typeSymbol != peer ||
            expectedValue.isEmpty ||
            expectedValue.get =:!= value ||
            tpe <:!< (types.from mapArgs { _ => List(placedValue, placedPeer) }))
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

  private def typecheckInPositionContext(expr: Tree, pos: Position) = {
    object traverser extends Traverser {
      var result = EmptyTree
      var blocks = 0

      def process(trees: List[Tree], collectArguments: Boolean): List[Tree] = {
        def min(tree: Tree): Int =
          math.min(
            if (tree.children.nonEmpty) min(tree.children.head) else Int.MaxValue,
            if (tree.pos != NoPosition) tree.pos.point else Int.MaxValue)

        val index = trees indexWhere { tree => tree.pos != NoPosition && min(tree) > pos.point }

        val count = if (index == -1) trees.size - 1 else if (index > 0) index - 1 else 0

        val implicitArguments =
          if (collectArguments)
            trees collect { case tree @ ValDef(mods, name, tpt, EmptyTree)
                if (mods hasFlag Flag.PARAMACCESSOR) && (mods hasFlag Flag.IMPLICIT) =>
              treeCopy.ValDef(tree,
                mods withoutFlags (Flag.PARAMACCESSOR | Flag.PRIVATE | Flag.LOCAL),
                name,
                tpt,
                q"null.asInstanceOf[$tpt]")
            }
          else
            List.empty

        val imports = trees take count collect { case tree: Import => tree }

        result = EmptyTree
        if (count >= 0)
          traverse(trees(count))

        if (result.nonEmpty)
          implicitArguments ++ imports :+ result
        else
          implicitArguments ++ imports :+ expr
      }

      override def traverse(tree: Tree): Unit = tree match {
        case tree @ ClassDef(mods, _, tparams, impl) if blocks == 0 =>
          val body = process(impl.body, collectArguments = true)
          val name = TypeName(s"$$loci$$impl$$dummy$$${tree.name}")
          result = treeCopy.ClassDef(tree, mods mapAnnotations { _ => List.empty }, name, tparams,
            treeCopy.Template(impl, List.empty, noSelfType, body))

        case tree @ ModuleDef(mods, _, impl) if blocks == 0 =>
          val body = process(impl.body, collectArguments = false)
          val name = TermName(s"$$loci$$impl$$dummy$$${tree.name}")
          result = treeCopy.ModuleDef(tree, mods mapAnnotations { _ => List.empty }, name,
            treeCopy.Template(impl, List.empty, noSelfType, body))

        case tree: ImplDef =>
          val body = process(tree.impl.body, collectArguments = false)
          result = Block(body dropRight 1, body.last)

        case tree: Block =>
          blocks += 1
          val stats = process(tree.stats :+ tree.expr, collectArguments = false)
          blocks -= 1
          result = treeCopy.Block(tree, stats dropRight 1, stats.last)

        case _ =>
          super.traverse(tree)
      }
    }

    object transformer extends Transformer {
      var expressionFound = false

      override def transform(tree: Tree): Tree = tree match {
        case This(name) if name.toString startsWith "$loci$impl$dummy$" =>
          This(TypeName(name.toString.drop(17)))

        case tree: ImplDef if !expressionFound =>
          tree.impl.body.lastOption.fold(EmptyTree) { transform }

        case tree: Block if !expressionFound =>
          transform(tree.expr)

        case _ =>
          expressionFound = true
          super.transform(tree)
      }
    }

    traverser traverse c.macroApplication

    transformer transform c.typecheck(traverser.result, silent = true) orElse
    c.typecheck(expr, silent = true)
  }

  private def extractTransmittable(resolution: Tree) = resolution match {
    case q"new $_[..$_]($expr)" if expr.tpe <:< types.transmittable =>
      expr
    case q"$_[..$_]($_[..$_]($expr))" if expr.tpe <:< types.transmittable =>
      expr
    case q"$_[..$_](new $_[..$_]($expr))" if expr.tpe <:< types.transmittable =>
      expr
    case q"$_[..$_]($_[..$_]($_[..$_]($expr)))" if expr.tpe <:< types.transmittable =>
      expr
    case _ =>
      EmptyTree
  }

  private def methodSignature(symbol: MethodSymbol, returnType: Type) = {
    val name = symbol.name
    val args = (symbol.paramLists map { paramList =>
      (paramList map { _.info.erasure.typeSymbol.fullName }).mkString("(", ",", ")")
    }).mkString
    val result = returnType.erasure.typeSymbol.fullName
    s"$name$args:$result"
  }

  private def memberType(tpe: Type, name: Name) = {
    val symbol = tpe member name

    symbol.info.asSeenFrom(tpe.underlying, symbol.owner) match {
      case TypeBounds(lo, hi) if lo =:= definitions.NothingTpe => hi
      case TypeBounds(lo, hi) if hi =:= definitions.AnyTpe => lo
      case tpe => tpe
    }
  }

  private object DummyTransmittable extends Enumeration {
    val NonParameter, Parameter, None = Value

    def apply(tree: Tree) = {
      val trees = tree collect {
        case q"$expr[$tpt](...$_)" if expr.symbol.owner == symbols.transmittableDummy =>
          tpt
      }

      if (trees.isEmpty)
        None
      else if (trees forall { _.tpe.typeSymbol.isParameter })
        Parameter
      else
        NonParameter
    }
  }

  object PlacedValues {
    def resolve(path: String, symbol: Symbol, pos: Position): (TermName, Type, Option[Type]) =
      cache.getOrElseUpdate(path -> symbol, {
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

        val info =
          if (symbol.owner == module.classSymbol)
            module.classSymbol.selfType
          else
            symbol.owner.info

        (info.members.sorted
          collectFirst {
            case symbol @ PlacedValueSymbol(`signature`, argInfo, _) =>
              (symbol.name.toTermName,
               argInfo.fold(definitions.UnitTpe) { _.base },
               peerType)
          }
          getOrElse c.abort(pos, s"Could not find remote accessor for placed $symbol"))
      })

    def makeResolvable(path: String, symbol: Symbol, name: TermName, tpe: Type, subjective: Option[Type]): Unit =
      cache += path -> symbol -> ((name, tpe, subjective))

    private val cache: mutable.Map[(String, Symbol), (TermName, Type, Option[Type])] =
      RemoteAccess.placedValueCache.get(c).cache
  }

  object MarshallableSymbol {
    def unapply(symbol: Symbol): Option[(MarshallableInfo, TermName)] =
      cache.getOrElse(symbol, {
        if (symbol.isMethod && (symbol.name.toString startsWith "$loci$mar$")) {
          val method = symbol.asMethod
          val tpe = method.info.finalResultType

          if (tpe <:< types.marshallable)
            method.allAnnotations map { _.tree } collectFirst {
              case tree @ Apply(_, List(Literal(Constant(signature: Int))))
                  if tree.tpe <:< types.marshallableInfo =>
                val Seq(base, result, proxy) = tpe.typeArgs: @unchecked
                val Seq(intermediate) = tree.tpe.typeArgs: @unchecked
                (MarshallableInfo(
                  base.asSeenFrom(module.classSymbol),
                  intermediate.asSeenFrom(module.classSymbol),
                  result.asSeenFrom(module.classSymbol),
                  proxy.asSeenFrom(module.classSymbol),
                  if (method.allAnnotations exists { _.tree.tpe <:< types.abstractValue })
                    None
                  else
                    Some(signature)),
                  method.name)
            }
          else
            None
        }
        else
          None
      })

    private val cache = mutable.Map.empty[Symbol, Option[(MarshallableInfo, TermName)]]
  }

  object PlacedValueSymbol {
    def unapply(symbol: Symbol): Option[(String, Option[MarshallableInfo], Option[MarshallableInfo])] =
      cache.getOrElse(symbol, {
        if (symbol.isMethod && (symbol.name.toString startsWith "$loci$val$")) {
          val method = symbol.asMethod
          val tpe = method.info.finalResultType

          if (tpe <:< types.placedRuntimeValue)
            (method.allAnnotations map { _.tree } collectFirst {
              case tree @ Apply(_, List(Literal(Constant(signature: String)), arg, res))
                  if tree.tpe <:< types.placedRuntimeValueInfo =>
                def info(tree: Tree) = {
                  val symbol =
                    if (tree.symbol != null && tree.symbol.owner.isClass)
                      tree.symbol.owner.asClass.selfType member tree.symbol.name
                    else
                      NoSymbol

                  (tree, symbol) match {
                    case (Literal(Constant(null)), _) => Some(None)
                    case (_, MarshallableSymbol(info, _)) => Some(Some(info))
                    case _ => None
                  }
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

    private val cache = mutable.Map.empty[Symbol, Option[(String, Option[MarshallableInfo], Option[MarshallableInfo])]]
  }
}
