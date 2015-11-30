package retier
package impl

import RemoteRef._
import network.ProtocolInfo

private final case class RemoteRefImpl(peerType: PeerType, id: Long,
    protocol: ProtocolInfo)
  extends RemoteImplBase[Nothing]

object RemoteRef {
  type RemoteRef = transmission.RemoteRef

  private[impl] def create[R <: Peer: PeerTypeTag](id: Long,
      protocol: ProtocolInfo): Remote[R] =
    RemoteRefImpl(peerTypeOf[R], id, protocol)

  private[impl] def create(peerType: PeerType, id: Long,
      protocol: ProtocolInfo): RemoteRef =
    RemoteRefImpl(peerType, id, protocol)

  implicit class RemoteRefOps(remoteRef: RemoteRef) {
    def peerType: PeerType = remoteRef match {
      case RemoteRefImpl(peerType, _, _) => peerType
      case _ => throwRetierImplementationError(remoteRef)
    }

    def id: Long = remoteRef match {
      case RemoteRefImpl(_, id, _) => id
      case _ => throwRetierImplementationError(remoteRef)
    }

    def asRemote[R <: Peer: PeerTypeTag]: Option[Remote[R]] = remoteRef match {
      case remote @ RemoteRefImpl(tpe, _, _) =>
        if (tpe <= peerTypeOf[R]) Some(remote)
        else None
      case _ => throwRetierImplementationError(remoteRef)
    }

    def asRemote(peerType: PeerType): Option[RemoteRef] = remoteRef match {
      case remote @ RemoteRefImpl(tpe, _, _) =>
        if (tpe <= peerType) Some(remote)
        else None
      case _ => throwRetierImplementationError(remoteRef)
    }

    private def throwRetierImplementationError(ref: Any) =
      throw new RetierImplementationError(
        s"invalid remote reference implementation: ${className(ref)}")
  }
}
