package retier
package tcp

import network.ConnectionListener
import network.ConnectionRequestor
import network.ProtocolInfo

abstract case class TCP private[TCP] (host: String, port: Int)
    extends ProtocolInfo {

  private def readResolve(): Object =
    TCP.createProtocolInfo(host, port)
  def copy(host: String = host, port: Int = port): TCP =
    TCP.createProtocolInfo(host, port)

  val isEncrypted = false
  val isProtected = false
  val isAuthenticated = false
  val identification = None
}

object TCP {
  def apply(port: Int, interface: String = "localhost"): ConnectionListener =
    new TCPConnectionListener(port, interface)
  def apply(host: String, port: Int): ConnectionRequestor =
    new TCPConnectionRequestor(host, port)
  def createProtocolInfo(host: String, port: Int) =
    new TCP(host, port) { }
}
