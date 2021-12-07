package loci
package communicator
package ws.akka

import play.api.mvc.WebSocket

import scala.annotation.compileTimeOnly

@compileTimeOnly("Akka Play WebSocket communicator only available on the JVM")
object WebSocketHandler {
  private def ??? =
    sys.error("Akka Play WebSocket communicator only available on the JVM")

  def apply(): Listener[WS] with WebSocketHandler = ???
  def apply(properties: WS.Properties): Listener[WS] with WebSocketHandler = ???

  object Secure {
    def apply(): Listener[WS.Secure] with WebSocketHandler = ???
    def apply(properties: WS.Properties): Listener[WS.Secure] with WebSocketHandler = ???
  }
}

@compileTimeOnly("Akka Play WebSocket communicator only available on the JVM")
trait WebSocketHandler extends WebSocket {
  def apply(authenticatedName: String): WebSocket
  def apply(authenticatedName: Option[String]): WebSocket
}
