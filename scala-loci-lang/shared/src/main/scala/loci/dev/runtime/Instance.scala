package loci.dev
package runtime

import loci.communicator._
import loci.impl.RemoteConnections
import loci.messaging.ConnectionsBase.Protocol
import loci.transmitter.RemoteRef

import scala.concurrent.{ExecutionContext, Future}

abstract class Instance[P] extends loci.dev.Instance[P](0) {
  val values: PlacedValues
  def terminate(): Unit
}

object Instance {
  type Connections =
    Map[Peer.Signature, (List[Listener[Protocol]], List[Connector[Protocol]])]

  type SystemFactory =
    (ExecutionContext, RemoteConnections, Seq[RemoteRef], Seq[Future[RemoteRef]]) => System

  def start[P](
      signature: Peer.Signature,
      ties: Map[Peer.Signature, Peer.Tie],
      context: ExecutionContext,
      connect: Connections,
      system: SystemFactory): Instance[P] = ???
}
