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

    object baseReferencProcessor extends Transformer {
      override def transform(tree: Tree) = tree match {
        case q"$_[..$_](...$exprss)" if tree.symbol == symbols.placedBase =>
          exprss.head.head match {
            case q"$_.$tname[..$tpts](...$exprss)" =>
              super.transform(q"super.$tname[..$tpts](...$exprss)")
            case _ =>
              super.transform(exprss.head.head)
          }

        case _ =>
          super.transform(tree)
      }
    }

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
        (overridingExpr: Tree, stat: PlacedStatement): Option[TermSymbol] =
      overridingExpr match {
        case expr if expr.symbol == symbols.placedOverriding =>
          val q"$exprBase.$_[..$_].$_[..$_](...$exprss)" = expr
          val identifier = reduceEtaExpansion(exprss.head.head)

          if (stat.declTypeTree.isEmpty)
            c.abort(identifier.pos, "overriding must be part of a declaration")

          identifier match {
            case q"$_.$tname" =>
              Some(identifier.symbol.asTerm)
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

          val placedExpr =
            if (symbols.placedAbstract == expr.symbol) {
              if (!peerSymbol.isAbstract)
                c.abort(tree.pos,
                  "undefined members not allowed for " +
                  "concrete (non-abstract) peer types")

              EmptyTree
            }
            else {
              val q"(..$_) => $exprPlaced" = exprss.head.head
              exprPlaced
            }

          (processOverridingDeclaration(exprBase, stat), placedExpr)
        }
        else
          (overridingDecl, expr)

        if (expr.symbol == symbols.placedIssuedApply && declTypeTree.isEmpty)
          c.abort(tree.pos, "issuing must be part of a declaration")

        // construct new placed statement
        // with the actual placed expression syntactic construct removed
        PlacedStatement(tree, peerSymbol, exprType, declTypeTree,
          processedOverridingDecl, baseReferencProcessor transform placedExpr)
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
