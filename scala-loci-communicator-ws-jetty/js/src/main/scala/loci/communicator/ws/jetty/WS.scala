package loci
package communicator
package ws.jetty

import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.websocket.servlet.ServletUpgradeRequest

import scala.annotation.compileTimeOnly
import scala.concurrent.duration._

trait WS extends
    Protocol with
    SetupInfo with
    SecurityInfo with
    SymmetryInfo with Bidirectional {
  val path: String
  val host: Option[String]
  val port: Option[Int]
  val request: Option[ServletUpgradeRequest]

  override def toString = s"WS($path, $host, $port)"
}

@compileTimeOnly("Jetty WebSocket communicator only available on the JVM")
object WS extends WSSetupFactory {
  def unapply(ws: WS) = Some((ws.path, ws.host, ws.port))

  case class Properties(
    heartbeatDelay: FiniteDuration = 3.seconds,
    heartbeatTimeout: FiniteDuration = 10.seconds)

  private def ??? =
    sys.error("Jetty WebSocket communicator only available on the JVM")

  def apply(context: ServletContextHandler, pathspec: String): Listener[WS] = ???
  def apply(context: ServletContextHandler, pathspec: String, properties: Properties): Listener[WS] = ???

  def apply(url: String): Connector[WS] = ???
  def apply(url: String, properties: Properties): Connector[WS] = ???

  trait Secure extends WS with communicator.Secure {
    override def toString = s"WS.Secure($path, $host, $port)"
  }

  object Secure {
    def unapply(ws: Secure) = Some((ws.path, ws.host, ws.port))

    def apply(url: String): Connector[WS.Secure] = ???
    def apply(url: String, properties: Properties): Connector[WS.Secure] = ???
  }
}
