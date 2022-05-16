package loci
package communicator
package ws.javalin

import scala.annotation.compileTimeOnly
import scala.concurrent.duration._

trait WS
    extends Protocol
    with SetupInfo
    with SecurityInfo
    with SymmetryInfo with Bidirectional {
  val path: String
  val host: Option[String]
  val port: Option[Int]
  val context: AnyRef

  override def toString = s"WS($path, $host, $port)"
}

@compileTimeOnly("Javalin WebSocket communicator only available on the JVM")
object WS extends WSSetupFactory {
  def unapply(ws: WS) = Some((ws.path, ws.host, ws.port))

  case class Properties(
    heartbeatDelay: FiniteDuration = 3.seconds,
    heartbeatTimeout: FiniteDuration = 10.seconds)

  def apply(javalin: AnyRef, path: String): Listener[WS] = unavailable
  def apply(javalin: AnyRef, path: String, properties: Properties): Listener[WS] = unavailable
}
