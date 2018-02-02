package loci
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
    RemoteExpressionProcessor with
    FromExpressionProcessor with
    ToExpressionProcessor with
    MetaPeerExpressionProcessor with
    TransmissionGenerator with
    RemoteInterfaceGenerator with
    ProxyGenerator with
    OverrideBridgeGenerator with
    PeerTypeTagGenerator with
    PeerTieMultiplicityGenerator with
    PeerImplementationGenerator with
    OutputGenerator {

  def process(state: CodeWrapper[c.type]): CodeWrapper[c.type] = {
    val aggregator =
      Aggregator.create(state.body.zipWithIndex map InputStatement.tupled) add
      List(EnclosingContext(state.name, state.bases)) aggregate
      collectPeerDefinitions aggregate
      collectStatements aggregate
      validateNames aggregate
      processPlacedExpressions aggregate
      processRemoteExpressions aggregate
      processFromExpressions aggregate
      processToExpressions aggregate
      processMetaPeerExpressions aggregate
      generateTransmissions aggregate
      generateRemoteInterfaces aggregate
      generateProxies aggregate
      generateOverrideBridge aggregate
      generatePeerTypeTags aggregate
      generatePeerTieMultiplicities aggregate
      generatePeerImplementations aggregate
      generateOutput

    state replaceBody (aggregator.all[OutputStatement] map { _.stat })
  }
}
