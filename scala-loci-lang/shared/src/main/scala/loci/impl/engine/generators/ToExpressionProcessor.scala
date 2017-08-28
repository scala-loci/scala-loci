package loci
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait ToExpressionProcessor { this: Generation =>
  val c: Context
  import c.universe._

  val processToExpressions = UniformAggregation[PlacedStatement] {
      aggregator =>

    echo(verbose = true, " Processing to expressions")

    val stats = aggregator.all[PlacedStatement] map { stat =>
      stat.copy(expr = toExpressionProcessor transform stat.expr)
    }

    echo(verbose = true,
      s"  [${stats.size} placed statements generated, existing replaced]")

    aggregator replace stats
  }

  private object toExpressionProcessor extends Transformer {
    def processToExpression(exprssValue: List[List[Tree]],
        exprssPeer: List[List[Tree]]) = {
      val value = exprssValue.head.head
      val peer = exprssPeer.head.head

      if ((types.controlledSubjectivePlacing exists { value.tpe <:< _ }) &&
          (types.bottom forall { value.tpe <:!< _ }))
        q"$value($peer)"
      else
        q"$value"
    }

    override def transform(tree: Tree) = tree match {
      case q"(new $expr[..$_](...$exprssValue)).$tname(...$exprssPeer)"
          if expr.tpe <:< types.toExpression &&
             tname.encodedName == names.to.encodedName =>
        super.transform(processToExpression(exprssValue, exprssPeer))

      case q"$expr[..$_](...$exprssValue).$tname(...$exprssPeer)"
          if expr.symbol == symbols.toExpression &&
             tname.encodedName == names.to.encodedName =>
        super.transform(processToExpression(exprssValue, exprssPeer))

      case _ =>
        super.transform(tree)
    }
  }
}
