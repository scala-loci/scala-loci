package loci
package communicator
package ws.akka

import play.api.mvc.WebSocket

object WebSocketHandler {
  def apply(): Listener[WS] with WebSocketHandler = WS.???
  def apply(properties: WS.Properties): Listener[WS] with WebSocketHandler = WS.???

  object Secure {
    def apply(): Listener[WS.Secure] with WebSocketHandler = WS.???
    def apply(properties: WS.Properties): Listener[WS.Secure] with WebSocketHandler = WS.???
  }
}

trait WebSocketHandler extends WebSocket {
  def apply(authenticatedName: String): WebSocket
  def apply(authenticatedName: Option[String]): WebSocket
}
