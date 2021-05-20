package loci
package language

import scala.reflect.macros.blackbox

object GatewayResolutionFailure {
  def selectDynamic(c: blackbox.Context)(key: c.Tree): c.Tree = fail(c)(key)
  def updateDynamic(c: blackbox.Context)(key: c.Tree)(value: c.Tree): c.Tree = fail(c)(key)
  def applyDynamic(c: blackbox.Context)(key: c.Tree)(args: c.Tree*): c.Tree = fail(c)(key)

  def fail(c: blackbox.Context)(key: c.Tree): c.Tree = {
    import c.universe._

    val gatewayType = c.prefix.tree.tpe.baseType(symbolOf[Gateway[_]])

    val peerTree = c.prefix.tree match {
      case q"$_[$tpt]" => tpt
      case _ => EmptyTree
    }

    val peerPos = peerTree match {
      case tree: TypeTree if tree.original != null => tree.original.pos
      case tree => tree.pos
    }

    if (peerPos == NoPosition)
      c.abort(c.prefix.tree.pos, "Illegal use of multitier construct: Try using `remote[P]` where `P` is a peer type")

    val localPeerType = {
      val tpe = typeOf[Placement.Context[_]]

      val tree = c inferImplicitValue tpe
      if (tree.isEmpty)
        c.abort(c.prefix.tree.pos, "Expression must be placed on a peer")

      tree.tpe.finalResultType.typeArgs.head
    }

    val tieType = {
      val ExistentialType(quantified, TypeRef(pre, sym, args)) =
        typeOf[Tie[Any, Any, _]]: @unchecked
      val tpe = internal.existentialType(quantified,
        internal.typeRef(pre, sym, List(localPeerType, peerTree.tpe, args.last)))

      val tree = c inferImplicitValue tpe
      if (tree.isEmpty)
        c.abort(c.prefix.tree.pos, s"No tie specified from ${peerTree.tpe} to $localPeerType")

      tree.tpe.finalResultType.typeArgs.last
    }

    val method = key match {
      case Literal(Constant(constant)) => constant.toString
      case _ => key.toString
    }

    c.abort(c.enclosingPosition,
      s"No peer gateway method $method for peer ${peerTree.tpe} " +
      s"(with ${tieType.typeSymbol.name.toString.toLowerCase} tie from local peer $localPeerType)" +
      utility.implicitHints.conversions(c)(gatewayType))
  }
}
