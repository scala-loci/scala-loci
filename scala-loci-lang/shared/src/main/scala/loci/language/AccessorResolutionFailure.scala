package loci
package language

import transmitter.transmittable.Transmittable

import scala.reflect.NameTransformer
import scala.reflect.macros.blackbox

object AccessorResolutionFailure {
  def selectDynamic(c: blackbox.Context)(key: c.Tree): c.Tree = fail(c)(key)
  def updateDynamic(c: blackbox.Context)(key: c.Tree)(value: c.Tree): c.Tree = fail(c)(key)
  def applyDynamic(c: blackbox.Context)(key: c.Tree)(args: c.Tree*): c.Tree = fail(c)(key)

  def fail(c: blackbox.Context)(key: c.Tree): c.Tree = {
    import c.universe._

    val placedValueType = c.prefix.tree.tpe.baseType(symbolOf[PlacedValue[_, _]])

    if (placedValueType == null || placedValueType.typeArgs.size != 2)
      c.abort(c.prefix.tree.pos, "No placed type")

    val method = key match {
      case Literal(Constant(constant)) => constant.toString
      case _ => key.toString
    }

    val desugaredAssignment =
      if (method.length > 1 &&
          method != "<=" && method != ">=" &&  method != "!=" &&
          method.last == '=' && method.head != '=' &&
          (method forall { c => c.toString != (NameTransformer encode c.toString) })) {
        val operator = TermName(NameTransformer encode method.dropRight(1))
        if ((c.prefix.tree.tpe member operator) != NoSymbol) {
          def pairsToNamedArgs(exprss: List[List[Tree]]) = {
            val argss = exprss map {
              _ map {
                case Apply(_, List(Literal(Constant(name: String)), value)) =>
                  val q"apply($expr)" = q"apply(${TermName(name)} = $value)": @unchecked
                  expr
                case _ =>
                  EmptyTree
              }
            }

            if (argss forall { _ forall { _.nonEmpty } })
              Some(argss)
            else
              None
          }

          def isSelfSelector(tree: Tree) = tree match {
            case This(_) | Super(_, _) => true
            case _ => false
          }

          c.macroApplication match {
            case q"$expr.$name.applyDynamic($_)(...$exprss)" if !isSelfSelector(expr) =>
              q"val x = $expr; x.$name = x.$name.$operator(...$exprss)"
            case q"$expr.applyDynamic($_)(...$exprss)" =>
              q"$expr = $expr.$operator(...$exprss)"
            case q"$expr.$name.applyDynamicNamed($_)(...$exprss)" if !isSelfSelector(expr) =>
              pairsToNamedArgs(exprss).fold(???) { argss =>
                q"val x = $expr; x.$name = x.$name.$operator(...$argss)"
              }
            case q"$expr.applyDynamicNamed($_)(...$exprss)" =>
              pairsToNamedArgs(exprss).fold(???) { argss =>
                q"$expr = $expr.$operator(...$argss)"
              }
            case _ =>
              EmptyTree
          }
        }
        else
          EmptyTree
      }
      else
        EmptyTree

    desugaredAssignment orElse {
      val Seq(valueType, peerType) = placedValueType.typeArgs: @unchecked

      val underlyingValueType = valueType match {
        case TypeRef(_, sym, List(valueType)) if sym == symbolOf[Local[_]] => valueType
        case _ => valueType
      }

      val message = s"Value $method is not a member of $underlyingValueType"

      val localPeerType = {
        val tpe = typeOf[Placement.Context[_]]

        val tree = c inferImplicitValue tpe
        if (tree.isEmpty)
          c.abort(c.prefix.tree.pos, "Expression must be placed on a peer")

        tree.tpe.finalResultType.typeArgs.head
      }

      val (isSelection, selectedValueType, tieType) = {
        val selection =
          valueType <:< typeOf[Placed.Selected.Single[_]] ||
          valueType <:< typeOf[Placed.Selected.Multiple[_]]

        val ExistentialType(quantified, TypeRef(pre, sym, args)) =
          typeOf[Multiplicity[Any, Any, Any, _, _]]: @unchecked
        val tpe = internal.existentialType(quantified,
          internal.typeRef(pre, sym, localPeerType :: peerType :: valueType :: args.takeRight(2)))

        val tree = c inferImplicitValue tpe
        if (tree.isEmpty) {
          if (!selection) {
            val ExistentialType(quantified, TypeRef(pre, sym, args)) =
              typeOf[Tie[Any, Any, _]]: @unchecked
            val tpe = internal.existentialType(quantified,
              internal.typeRef(pre, sym, List(localPeerType, peerType, args.last)))

            val tree = c inferImplicitValue tpe
            if (tree.isEmpty)
              c.abort(c.prefix.tree.pos, s"$message. For remote access: " +
                s"No tie specified from $peerType to $localPeerType")
          }

          c.abort(c.prefix.tree.pos, s"$message. For remote access: " +
            "Could not resolve multiplicity for remote value through selection type or tie")
        }

        val finalResultType = tree.tpe.finalResultType
        (selection, finalResultType.typeArgs(3), finalResultType.typeArgs(4))
      }

      val subjectiveValueType = {
        val ExistentialType(quantified, TypeRef(pre, sym, args)) =
          typeOf[Subjectivity[Any, _]]: @unchecked
        val tpe = internal.existentialType(quantified,
          internal.typeRef(pre, sym, List(selectedValueType, args.last)))

        val tree = c inferImplicitValue tpe
        if (tree.isEmpty)
          c.abort(c.prefix.tree.pos, s"$message. For remote access: " +
            "Could not resolve subjectivity for remote value")

        tree.tpe.finalResultType.typeArgs.last
      }

      val ExistentialType(quantified, TypeRef(pre, sym, args)) =
        typeOf[Transmittable.Resolution[Any, _, _, _, _]]: @unchecked
      val tpe = internal.existentialType(quantified,
        internal.typeRef(pre, sym, subjectiveValueType :: args.tail))

      c.inferImplicitValue(tpe, silent = false)

      c.abort(c.enclosingPosition,
        s"No remote accessor method $method for peer $peerType " +
        s"(with ${tieType.typeSymbol.name.toString.toLowerCase} ${if (isSelection) "selection" else "tie"} " +
        s"from local peer $localPeerType)" +
        utility.implicitHints.conversions(c)(placedValueType))
    }
  }
}
