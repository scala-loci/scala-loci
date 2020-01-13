package loci
package runtime

import java.util.concurrent.atomic.AtomicBoolean

import loci.communicator.ProtocolCommon

import scala.annotation.compileTimeOnly

object Remote {
  case class Reference(
    id: Long,
    signature: Peer.Signature)(
    val protocol: ProtocolCommon,
    private[runtime] val remoteConnections: RemoteConnections)
      extends loci.Remote.Reference[Nothing] {

    private[runtime] val isConnected = new AtomicBoolean(true)
    private[runtime] val isAuthenticated = new AtomicBoolean(protocol.authenticated)
    private[runtime] val doDisconnected = Notice.Steady[Unit]

    def asReference = this
    def authenticated = isAuthenticated.get
    def authenticate() = isAuthenticated set true
    def connected = isConnected.get
    def disconnect() = remoteConnections disconnect this
    val disconnected = doDisconnected.notice

    override def toString: String = s"remote#$id($signature[$protocol])"
  }

  @compileTimeOnly("Call only allowed in multitier code. Use `remote.asRemote[P]` instead.")
  def cast[P](reference: loci.Remote.Reference[_]): Option[Remote[P]] = ???
}
