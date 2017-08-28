package loci
package impl

import RemoteRef._
import communicator.ProtocolCommon
import java.util.concurrent.atomic.AtomicBoolean

private final case class RemoteRefImpl(peerType: PeerType, id: Long,
    protocol: ProtocolCommon)(
    val isConnected: AtomicBoolean,
    val isAuthenticated: AtomicBoolean,
    val remoteConnections: RemoteConnections) extends Remote[Nothing] {
  val doDisconnected = Notifier[Unit]

  def authenticated = isAuthenticated.get
  def authenticate() = isAuthenticated set true
  def connected = isConnected.get
  def disconnect() = remoteConnections disconnect this
  val disconnected = doDisconnected.notification
}

object RemoteRef {
  type RemoteRef = transmitter.RemoteRef

  private[impl] def create[R <: Peer: PeerTypeTag](id: Long,
      protocol: ProtocolCommon, remoteConnections: RemoteConnections): Remote[R] =
    RemoteRefImpl(
      peerTypeOf[R], id, protocol)(
      new AtomicBoolean(true), new AtomicBoolean(protocol.authenticated),
      remoteConnections)

  private[impl] def create(peerType: PeerType, id: Long,
      protocol: ProtocolCommon, remoteConnections: RemoteConnections): RemoteRef =
    RemoteRefImpl(
      peerType, id, protocol)(
      new AtomicBoolean(true), new AtomicBoolean(protocol.authenticated),
      remoteConnections)

  implicit class RemoteRefOps(remote: RemoteRef) {
    def peerType: PeerType = remote match {
      case RemoteRefImpl(peerType, _, _) => peerType
      case _ => throwLociImplementationError(remote)
    }

    def id: Long = remote match {
      case RemoteRefImpl(_, id, _) => id
      case _ => throwLociImplementationError(remote)
    }

    def doDisconnected(): Unit = remote match {
      case remote @ RemoteRefImpl(_, _, _) =>
        remote.isConnected set false
        remote.doDisconnected()
      case _ => throwLociImplementationError(remote)
    }

    def asRemote[R <: Peer: PeerTypeTag]: Option[Remote[R]] = remote match {
      case remote @ RemoteRefImpl(tpe, _, _) =>
        if (tpe <= peerTypeOf[R]) Some(remote)
        else None
      case _ => throwLociImplementationError(remote)
    }

    def asRemote(peerType: PeerType): Option[RemoteRef] = remote match {
      case remote @ RemoteRefImpl(tpe, _, _) =>
        if (tpe <= peerType) Some(remote)
        else None
      case _ => throwLociImplementationError(remote)
    }

    private def throwLociImplementationError(ref: Any) =
      throw new LociImplementationError(
        s"invalid remote reference implementation: ${className(ref)}")
  }
}
