package retier
package impl
package engine

import generators._
import scala.reflect.macros.blackbox.Context

object CodeProcessor {
  def apply[C <: Context](c: C): CodeProcessor[c.type] =
    new CodeProcessor[c.type](c)
}

class CodeProcessor[C <: Context](val c: C) extends
    Generation with
    StatementCollector with
    PeerDefinitionCollector with
    PlacedExpressionsEraser {
  import c.universe._

  def process(state: CodeWrapper[c.type]): CodeWrapper[c.type] = {
    val aggregator =
      Aggregator.create(state.body map InputStatement) aggregate
      collectPeerDefinitions aggregate
      collectStatements aggregate
      erasePlacedExpressions


    // TODO: erase conversions on placed values before erasing placed expressions

    state
  }
}
