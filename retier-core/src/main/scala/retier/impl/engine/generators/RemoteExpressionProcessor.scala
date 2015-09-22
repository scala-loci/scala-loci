package retier
package impl
package engine.generators

import engine._
import scala.collection.mutable.ListBuffer
import scala.reflect.NameTransformer
import scala.reflect.macros.blackbox.Context

trait RemoteExpressionProcessor { this: Generation =>
  val c: Context
  import c.universe._
  import names._

  val processRemoteExpressions = UniformAggregation[PlacedStatement] {
      aggregator =>

    echo(verbose = true, " Processing remote expressions")

    val stats = aggregator.all[PlacedStatement] map { stat =>
      stat.copy(expr =
        new RemoteExpressionGenerator transform stat.expr)
    }

    echo(verbose = true,
      s"  [${stats.size} placed statements generated, existing replaced]")

    aggregator replace stats
  }

  private class RemoteExpressionGenerator extends Transformer {
    def processSelectionExpression(expr: Tree, selected: Tree) = {
      val (remoteExpr, selectionTree) = expr match {
        case tree @ q"$expr.$_[..$_](...$exprss)"
            if symbols.remoteOn contains tree.symbol =>
          (expr, Some(exprss.head))
        case _ =>
          (expr, None)
      }

      val q"$_[$tpt]" = remoteExpr
      val typeTree = markRetierSynthetic(
        tpt.typeTree(abortOnFailure = true),
        remoteExpr.pos)

      val fromExpression = markRetierSynthetic(
        internal setSymbol (q"`<FromExpression>`", symbols.fromExpression),
        remoteExpr.pos)

      selectionTree match {
        case Some(selectionTree) =>
          q"$fromExpression($selected) $from[$typeTree] (..$selectionTree)"
        case _ =>
          markRetierSynthetic(selected, recursive = false)
      }
    }

    override def transform(tree: Tree) = tree match {
      case q"$expr.$_[..$_](...$exprss)"
          if tree.symbol == symbols.remoteCall =>
        val call = super.transform(exprss.head.head)
        processSelectionExpression(expr, call)

      case q"$expr.$_[..$_](...$exprssIdentifier).$_[..$_](...$exprssValue)"
          if tree.symbol == symbols.remoteSet =>
        val value = exprssValue.head.head
        val identifier = exprssIdentifier.head.head

        val setter = identifier match {
          case q"$expr.$tname" =>
            if (!identifier.symbol.isTerm ||
                !identifier.symbol.asTerm.isGetter ||
                !identifier.symbol.asTerm.accessed.asTerm.isVar)
              c.abort(identifier.pos, "variable expected")

            val name = TermName(NameTransformer encode s"${tname.toString}_=")
            q"$expr.$name"

          case _ =>
            c.abort(identifier.pos, "identifier expected")
        }

        val TypeRef(pre, sym, List(_, peerType)) =
          identifier.tpe.widen.dealias
        val tpe =
          internal typeRef (pre, sym, List(definitions.UnitTpe, peerType))

        val call = super.transform(internal setType (q"$setter($value)", tpe))
        processSelectionExpression(expr, call)

      case _ =>
        super.transform(tree)
    }
  }
}
