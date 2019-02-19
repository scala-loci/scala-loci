package loci.dev
package language
package impl
package components

import scala.collection.mutable
import scala.reflect.macros.blackbox

object RemoteAccess extends Component.Factory[RemoteAccess](
    requires = Seq(ModuleInfo, Commons, Values)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new RemoteAccess(engine)
  def asInstance[C <: blackbox.Context] = { case c: RemoteAccess[C] => c }
}

class RemoteAccess[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq(
    Phase("remote:marshalling", createMarshallables, after = Set("values:validate"), before = Set("impls:lift")))

  val moduleInfo = engine.require(ModuleInfo)
  val commons = engine.require(Commons)
  val values = engine.require(Values)

  import engine._
  import engine.c.universe._
  import moduleInfo._
  import commons._
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
        decomposePlacementType(method.returnType, EmptyTree, method, method.pos) match {
          case Placed(_, tpe, _, modality) if modality != Modality.Local =>
            if (method.typeParams.nonEmpty)
              c.abort(method.pos, "Placed methods cannot have type parameters")

            val types = List(tpe) :: (method.paramLists map { _ map { _.info }})
            val typeViews = types map {
              _ map { tpe =>
                tpe.asSeenFrom(internal.thisType(module.classSymbol), tpe.typeSymbol.owner)
              }
            }

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

            if (modality == Modality.None)
              Some((method, res, tuple))
            else
              Some((method, res.typeArgs(1), tuple))

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

    val unresolvedTransmittables = mutable.ListBuffer.empty[Type]

    // resolve or construct marshallables and placed value info instances
    var marshallableIndex = 0
    var placedValueIndex = 0
    var failed = false
    val (marshallableNames, accessorValues) = (placedValues map {
      case (symbol, res, arg) =>
        val flags = Flag.SYNTHETIC | Flag.FINAL
        val signature = methodSignature(symbol, res)
        val hasArguments = arg =:!= definitions.UnitTpe
        val hasResult = res =:!= definitions.UnitTpe && res =:!= definitions.NothingTpe
        val definedArgTransmittable = definedTransmittables firstOfArgumentType arg
        val definedResTransmittable = definedTransmittables firstOfBaseType res
        val argTransmittableType = types.resolution mapArgs { args => List(arg, args(1), arg) ++ args.drop(3) }
        val resTransmittableType = types.resolution mapArgs { res :: _.tail }

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
               (accessorGeneration != AccessorGeneration.Deferred || definedTransmittable.nonEmpty)))
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
               definedResTransmittable.nonEmpty && definedArgTransmittable.nonEmpty))))
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

                val (argName, argMarshallable) =
                  if (hasArguments) marshallable(argInfo, argTree) else q"null" -> Seq.empty
                val (resName, resMarshallable) =
                  if (hasResult) marshallable(resInfo, resTree) else q"null" -> Seq.empty

                Right(argName -> resName) -> (argMarshallable ++ resMarshallable)
              }

              name match {
                case Left(name) =>
                  // reusing inherited placed value info if possible
                  symbol -> name -> seq

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
                        $signature,
                        ${symbol.isStable},
                        $argName,
                        $resName)"""
                  }

                  placedValueIndex += 1

                  symbol -> placedValueName -> (seq :+ placedValue)
              }

            // create statement to implicitly resolve the transmittable whose resolution failed
            // to communicate the failure to the developer
            case (resTransmittable, _) =>
              failed = true

              val transmittableName = TermName(s"$$loci$$resolution$$failure$$$moduleName")
              val message = s"${if (resTransmittable.isEmpty) res else arg} is not transmittable"
              val tpe = if (resTransmittable.isEmpty) resTransmittableType else argTransmittableType
              val rhs = q"${trees.implicitly}[${createTypeTree(tpe, symbol.pos)}]"

              val pos = (module.tree.impl.parents
                collectFirst { case tree if tree.symbol == symbol.owner => tree.pos }
                getOrElse symbol.pos)

              NoSymbol -> termNames.EMPTY -> Seq(
                atPos(pos) {
                  q"@${types.compileTimeOnly}($message) $flags def $transmittableName(): ${definitions.UnitTpe} = $rhs"
                },
                atPos(pos) {
                  q"$transmittableName()"
                })
          }
        else
          NoSymbol -> termNames.EMPTY -> Seq.empty
    }).unzip

    records ++ (accessorValues.flatten map { ModuleValue(NoSymbol, _) })
  }

  private def checkForTransmission[T](tree: Tree, peer: Symbol): Int = {
    val q"$_[..$_](...$exprss)" = tree

    if (exprss.size != 2 || exprss.head.size != 1)
      c.abort(tree.pos, "Invalid remote accessor: " +
        "Implicit conversion with implicit argument list required")

    var index = 0
    var count = 0
    var result = -1

    val arg = exprss(0).head

    exprss(1) foreach { expr =>
      if (expr.tpe real_<:< types.transmission) {
        val q"$_[..$params](...$_)" = expr
        if (params.size < 10)
          c.abort(tree.pos, "Unexpected transmission value")

        val value = params(9).tpe

        val Seq(placed, _, to, _) =
          extractTag(expr.tpe, types.transmission, tree.pos).typeArgs

        val expectedValue =
          decomposePlacementType(arg.tpe.widen, EmptyTree, arg.symbol, arg.pos) match {
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
