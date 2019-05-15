package loci.dev
package runtime

import loci.communicator._

final class GatewayConnection[R, M](
  peer: Peer.Signature,
  system: System)
    extends transmitter.Connection[R, M] {

  @inline private[dev] val remoteJoined: loci.Notification[Remote[R]] = ???
  @inline private[dev] val remoteLeft: loci.Notification[Remote[R]] = ???
  @inline private[dev] def multipleRemotes: Seq[Remote[R]] = ???
  @inline private[dev] def optionalRemote: Option[Remote[R]] = ???
  @inline private[dev] def singleRemote: Remote[R] = ???
  @inline private[dev] def connectRemote(connector: Connector[ProtocolCommon]): Unit = ???
}
