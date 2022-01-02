package loci
package runtime

import communicator.ProtocolCommon

import java.util.concurrent.atomic.AtomicBoolean

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

    def asReference: Reference = this
    def authenticated: Boolean = isAuthenticated.get
    def authenticate(): Unit = isAuthenticated.set(true)
    def connected: Boolean = isConnected.get
    def disconnect(): Unit = remoteConnections.disconnect(this)
    val disconnected: Notice.Steady[Unit] = doDisconnected.notice

    override def toString: String = s"remote#$id($signature[$protocol])"
  }

  /**
   * SelfReference as dummy to execute local remote calls on. Some operations that do not make sense to execute on
   * a SelfReference (compared to a real Reference) are throwing exceptions
   */
  final class SelfReference[P](
    val signature: Peer.Signature
  ) extends loci.SelfReference[P] {

    /**
     * SelfReference is (in a way) always connected
     */
    override def connected: Boolean = true

    override def canEqual(that: Any): Boolean = true

    override def asReference: SelfReference[P] = this

    override val disconnected: Notice.Steady[Unit] = Notice.Steady[Unit].notice

    override def authenticated: Boolean = throw IllegalSelfReferenceAccessException("authenticated")
    override def authenticate(): Unit = throw IllegalSelfReferenceAccessException("authenticate")
    override def protocol: ProtocolCommon = throw IllegalSelfReferenceAccessException("protocol")
    override def disconnect(): Unit = throw IllegalSelfReferenceAccessException("disconnect")

    override def toString: String = s"selfReference($signature)"
  }

  case class IllegalSelfReferenceAccessException(method: String) extends RuntimeException(
    s"Call of method $method is illegal on SelfReference. Ensure that remote is not a SelfReference before calling $method on it."
  )

  @compileTimeOnly("Call only allowed in multitier code. Use `remote.asRemote[P]` instead.")
  def cast[P](reference: loci.Remote.Reference[_]): Option[Remote[P]] = ???
}
