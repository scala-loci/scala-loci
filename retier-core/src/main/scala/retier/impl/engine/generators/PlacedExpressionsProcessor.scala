package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait PlacedExpressionsProcessor { this: Generation =>
  val c: Context
  import c.universe._

  val processPlacedExpressions = UniformAggregation[PlacedStatement] {
      aggregator =>

    echo(verbose = true, " Processing placed expressions")

    def reduceEtaExpansion(expr: Tree): Tree = {
      def reduceEtaExpansion(expr: Tree): Tree = {
        expr match {
          case q"{ (..$_) => $expr }" => reduceEtaExpansion(expr)
          case _ => expr
        }
      }

      expr match {
        case q"{ (..$_) => $expr }" => reduceEtaExpansion(expr) match {
          case q"$expr[..$_](...$_)" => expr
          case _ => expr
        }
        case _ => expr
      }
    }

    def processOverridingDeclaration
        (overridingExpr: Tree, stat: PlacedStatement): Option[TermName] =
      overridingExpr match {
        case expr if expr.symbol == symbols.placedOverriding =>
          val q"$exprBase.$_[..$_].$_[..$_](...$exprss)" = expr
          val identifier = reduceEtaExpansion(exprss.head.head)

          if (stat.declTypeTree.isEmpty)
            c.abort(identifier.pos, "overriding must be part of a declaration")

          identifier match {
            case q"$_.$tname" =>
              Some(tname)
            case _ =>
              c.abort(identifier.pos,
                "identifier expected " +
                "(note that `identifier _` syntax should be used)")
          }

        case _ =>
          None
      }

    def processPlacedExpression
        (stat: PlacedStatement): PlacedStatement = {
      import trees._

      val PlacedStatement(
        tree, peerSymbol, exprType, declTypeTree, overridingDecl, expr) = stat

      // process overriding declaration and extract placed expression
      val (processedOverridingDecl, placedExpr) =
        if (symbols.placed contains expr.symbol) {
          val (exprBase, exprss) = expr match {
            case q"$exprBase.$_[..$_].$_[..$_](...$exprss)"
                if expr.symbol == symbols.placedIssuedApply =>
              (exprBase, exprss)
            case q"$exprBase.$_[..$_](...$exprss)" =>
              (exprBase, exprss)
          }

          val q"(..$_) => $exprPlaced" = exprss.head.head

          (processOverridingDeclaration(exprBase, stat), exprPlaced)
        }
        else
          (overridingDecl, expr)

        // handle issued types
        val processedPlacedExpr =
          if (exprType <:< types.issuedControlled &&
              exprType <:!< types.issued &&
              (types.functionPlacing exists { placedExpr.tpe <:< _ }))
            q"""${markRetierSynthetic(ControlledIssuedValueCreate)}[
                ..${exprType.typeArgs}]($placedExpr)"""
          else if (exprType <:< types.issuedControlled &&
                   (types.issuedPlacing forall { placedExpr.tpe <:!< _ }))
            q"""${markRetierSynthetic(IssuedValueCreate)}[
                ..${exprType.typeArgs}]($placedExpr)"""
          else
            placedExpr

        if (expr.symbol == symbols.placedIssuedApply && declTypeTree.isEmpty)
          c.abort(tree.pos, "issuing must be part of a declaration")

        // construct new placed statement
        // with the actual placed expression syntactic construct removed
        PlacedStatement(tree, peerSymbol, exprType, declTypeTree,
          processedOverridingDecl, processedPlacedExpr)
    }

    def dropPrecedingGlobalCasts
        (stat: PlacedStatement): PlacedStatement =
      if (symbols.globalCasts contains stat.expr.symbol) {
        val q"$_(...$exprss)" = stat.expr
        stat.copy(expr = exprss.head.head)
      }
      else
        stat

    val stats =
      aggregator.all[PlacedStatement] map
      dropPrecedingGlobalCasts map
      processPlacedExpression

    echo(verbose = true,
      s"  [${stats.size} placed statements generated, existing replaced]")

    aggregator replace stats
  }
}
