package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait MetaPeerExpressionProcessor { this: Generation =>
  val c: Context
  import c.universe._

  val processMetaPeerExpressions = UniformAggregation[PlacedStatement] {
      aggregator =>

    echo(verbose = true, " Processing meta peer expressions")

    val stats = aggregator.all[PlacedStatement] map { stat =>
      stat.copy(expr = metaPeerExpressionProcessor transform stat.expr)
    }

    echo(verbose = true,
      s"  [${stats.size} placed statements generated, existing replaced]")

    aggregator replace stats
  }

  private object metaPeerExpressionProcessor extends Transformer {
    override def transform(tree: Tree) = tree match {
      case q"$expr[..$_](...$exprssValue)"
          if expr.symbol == symbols.peer =>
        q"${names.metapeer}"

      case _ =>
        super.transform(tree)
    }
  }
}
