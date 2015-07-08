package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait Generation {
  val c: Context
  import c.universe._


  val echo = Echo(c)


  object types {
    val peer = typeOf[Peer]
    val localOn = typeOf[`local on`[_, _]]
    val placing = typeOf[PlacingExpression[_]]
    val issuing = typeOf[IssuingExpression[_, _]]
    val overriding = typeOf[OverridingExpression[_]]
  }

  object symbols {
    val placedApply = types.placing member TermName("apply")
    val placedShared = types.placing member TermName("shared")
    val placedLocal = types.placing member TermName("local")
    val placedIssued = types.placing member TermName("issued")
    val placedIssuedApply = types.issuing member TermName("apply")
    val placedOverriding = types.overriding member TermName("overriding")

    val placed = Seq(placedApply, placedShared, placedLocal, placedIssuedApply)
  }


  case class InputStatement(stat: Tree)

  case class PeerDefinition(tree: Tree, peer: Type)

  case class PlacedStatement(tree: Tree, peerType: Type, exprType: Type,
      decl: Option[Tree], overridingDecl: Option[TermName], expr: Tree)

  case class NonPlacedStatement(tree: Tree)
}
