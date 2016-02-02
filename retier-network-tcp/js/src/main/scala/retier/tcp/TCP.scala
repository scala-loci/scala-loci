package retier
package tcp

import network.ConnectionListener
import network.ConnectionRequestor
import network.ConnectionFactory
import network.ProtocolInfo
import util.Attributes

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

object TCP extends ConnectionFactory {
  private def jvmOnly = sys.error(
      "A method of the TCP network provider has been called. " +
      "This is most likely because you tried to setup " +
      "the TCP network provider in a JavaScript environment. " +
      "The TCP network provider only supports the JVM.")

  def apply(port: Int, interface: String = "localhost"): ConnectionListener =
    jvmOnly
  def apply(host: String, port: Int): ConnectionRequestor =
    jvmOnly
  def createProtocolInfo(host: String, port: Int) =
    new TCP(host, port) { }

  def listener(config: String, attrs: Attributes) =
    None
  def requestor(url: String, attrs: Attributes) =
    None
}
