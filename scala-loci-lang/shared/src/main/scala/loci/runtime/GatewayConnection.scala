package loci
package runtime

import communicator._
import messaging._

final class GatewayConnection[R, M](
  peer: Peer.Signature,
  system: System)
    extends transmitter.Connection[R, M] {

  private[this] val peerId = (system, peer)

  @inline private[loci] def cache[B <: AnyRef](id: Any, body: => B): B =
    system.cache((peerId, id), body)

  @inline private[loci] val remoteJoined: Notice.Stream[Remote[R]] =
    system.remoteJoined(peer, Seq.empty, earlyAccess = false)

  @inline private[loci] val remoteLeft: Notice.Stream[Remote[R]] =
    system.remoteLeft(peer, Seq.empty, earlyAccess = false)

  @inline private[loci] def remoteReferences: Seq[Remote[R]] =
    system.remoteReferences(peer, Seq.empty, earlyAccess = false)

  @inline private[loci] def remoteConnect(connector: Connector[ConnectionsBase.Protocol]): Unit =
    system.connect(peer, connector)
}
