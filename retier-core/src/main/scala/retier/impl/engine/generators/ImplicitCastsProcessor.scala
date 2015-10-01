package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait ImplicitCastsProcessor { this: Generation =>
  val c: Context
  import c.universe._

  val processImplicitCasts = UniformAggregation[PlacedStatement] {
      aggregator =>

    echo(verbose = true, " Processing implicit casts for placed expressions")

    val stats = aggregator.all[PlacedStatement] map { stat =>
      stat.copy(expr = implicitCastsProcessor transform stat.expr)
    }

    echo(verbose = true,
      s"  [${stats.size} placed statements generated, existing replaced]")

    aggregator replace stats
  }

  private object implicitCastsProcessor extends Transformer {
    override def transform(tree: Tree) = tree match {
      case q"(new $expr[..$_](...$exprss)).$tname"
          if expr.tpe <:< types.valueOp && tname == names.value =>
        super.transform(exprss.head.head)

      case q"$expr[..$_](...$exprss).$tname"
          if expr.symbol == symbols.valueOp && tname == names.value =>
        super.transform(exprss.head.head)

      case tree @ q"$_(...$exprss)" if symbols.casts contains tree.symbol =>
        super.transform(exprss.head.head)

      case _ =>
        super.transform(tree)
    }
  }
}
