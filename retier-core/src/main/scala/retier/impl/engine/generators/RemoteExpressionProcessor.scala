package retier
package impl
package engine.generators

import engine._
import scala.collection.mutable.ListBuffer
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
    def processSelectionExpression(expr: Tree, selected: Tree,
        finalResultType: Type) = {
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

      val selectedPeerType = tpt.tpe
      val Seq(_, peerType) = finalResultType.typeArgs

      val fromExpression = markRetierSynthetic(
        internal setSymbol (q"`<FromExpression>`", symbols.fromExpression),
        remoteExpr.pos)

      selectionTree match {
        case Some(selectionTree) =>
          q"$fromExpression($selected) $from[$typeTree] (..$selectionTree)"
        case _ if selectedPeerType =:!= typeOf[Nothing] &&
                  selectedPeerType =:!= peerType =>
          q"$fromExpression($selected).$from[$typeTree]"
        case _ =>
          markRetierSynthetic(selected, recursive = false)
      }
    }

    override def transform(tree: Tree) = tree match {
      case q"$expr.$_[..$_](...$exprss)"
          if tree.symbol == symbols.remoteCall =>
        val call = super.transform(exprss.head.head)
        processSelectionExpression(expr, call, tree.tpe)

      case _ =>
        super.transform(tree)
    }
  }
}
