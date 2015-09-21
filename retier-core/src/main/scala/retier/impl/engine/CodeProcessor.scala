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
    PeerDefinitionCollector with
    StatementCollector with
    NamesValidator with
    PlacedExpressionsProcessor with
    FromExpressionProcessor with
    TransmissionGenerator with
    ProxyGenerator with
    OverrideBridgeGenerator with
    PeerTypeTagGenerator with
    PeerConnectionMultiplicityGenerator with
    PeerImplementationGenerator with
    OutputGenerator {
  import c.universe._

  def process(state: CodeWrapper[c.type]): CodeWrapper[c.type] = {
    val aggregator =
      Aggregator.create(state.body map InputStatement) add
      List(EnclosingContext(state.name, state.bases)) aggregate
      collectPeerDefinitions aggregate
      collectStatements aggregate
      validateNames aggregate
      processPlacedExpressions aggregate
      processFromExpressions aggregate
      generateTransmissions aggregate
      generateProxies aggregate
      generateOverrideBridge aggregate
      generatePeerTypeTags aggregate
      generatePeerConnectionMultiplicities aggregate
      generatePeerImplementations aggregate
      generateOutput


    // TODO: erase conversions on placed values before erasing placed expressions

    state replaceBody (aggregator.all[OutputStatement] map { _.stat })
  }
}
