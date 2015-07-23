package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait PlacedExpressionsEraser { this: Generation =>
  val c: Context
  import c.universe._

  val erasePlacedExpressions = UniformAggregation[PlacedStatement] {
      aggregator =>

    def processOverridingExpression
        (expr: Tree, stat: PlacedStatement): Option[TermName] =
      expr match {
        case expr if expr.symbol == symbols.placedOverriding =>
          val q"$exprBase.$_[..$_].$_[..$_](...$exprss)" = expr
          val identifier = exprss.head.head

          if (stat.declTypeTree.isEmpty)
            c.abort(identifier.pos, "overriding must be part of a declaration")

          identifier match {
            case q"$_.this.$tname" =>
              val Seq(declType, peerType) = identifier.tpe.typeArgs

              if (!(stat.exprType <:< declType))
                c.abort(identifier.pos, "overriding value of incompatible type")

              if (!(stat.peerType <:< peerType) || stat.peerType =:= peerType)
                c.abort(identifier.pos, "overriding value of non-base type")

              Some(tname)

            case _ =>
              c.abort(identifier.pos, "identifier of same scope expected")
              None
          }

        case _ =>
          None
      }

    def processPlacedExpression
        (stat: PlacedStatement): PlacedStatement =
      stat match {
        case stat @ PlacedStatement(
              tree, peerType, exprType, declTypeTree, _, expr)
            if symbols.placed contains expr.symbol =>
          val (exprBase, exprss) = expr match {
            case q"$exprBase.$_[..$_].$_[..$_](...$exprss)" =>
              (exprBase, exprss)
            case q"$exprBase.$_[..$_](...$exprss)" =>
              (exprBase, exprss)
          }

          val q"(..$_) => $exprPlaced" = exprss.head.head

          val (isIssued, placedAndIssuedExpr) =
            if (exprType <:< types.issuedControlled &&
                !(exprType <:< types.issued) &&
                exprPlaced.tpe <:< typeOf[_ => _]) {
              (true, q"""_root_.retier.impl.ControlledIssuedValue.create[
                         ..${exprType.typeArgs}]($exprPlaced)""")
            }
            else if (exprType <:< types.issuedControlled &&
                     !(exprPlaced.tpe <:< types.issuedControlled)) {
              (true, q"""_root_.retier.impl.IssuedValue.create[
                         ..${exprType.typeArgs}]($exprPlaced)""")
            }
            else
              (false, exprPlaced)

          if (isIssued && stat.declTypeTree.isEmpty)
            c.abort(tree.pos, "issuing must be part of a declaration")

          PlacedStatement(tree, peerType, exprType, declTypeTree,
            processOverridingExpression(exprBase, stat), placedAndIssuedExpr)

        case _ =>
          stat
      }

    def processGlobalCasts
        (stat: PlacedStatement): PlacedStatement =
      stat match {
        case stat @ PlacedStatement(
              tree, peerType, exprType, declTypeTree, overridingDecl, expr)
            if symbols.globalCasts contains expr.symbol =>
          val q"$_(...$exprss)" = expr

          PlacedStatement(tree, peerType, exprType, declTypeTree,
            overridingDecl, exprss.head.head)

        case _ =>
          stat
      }

    val stats = 
      aggregator.all[PlacedStatement] map
      processGlobalCasts map
      processPlacedExpression

    echo(
      verbose = true,
      s"Erased placed expressions " +
      s"(${stats.size} placed statements generated, existing replaced)")

    aggregator replace stats
  }
}
