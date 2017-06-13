package loci
package communicator
package ws.akka

import play.api.mvc.WebSocket

object WebSocketHandler {
  def apply(): Listener[WS] with WebSocketHandler =
    WSPlayListener[WS](WS.Properties())
  def apply(properties: WS.Properties): Listener[WS] with WebSocketHandler =
    WSPlayListener[WS](properties)

  object Secure {
    def apply(): Listener[WS.Secure] with WebSocketHandler =
      WSPlayListener[WS.Secure](WS.Properties())
    def apply(properties: WS.Properties): Listener[WS.Secure] with WebSocketHandler =
      WSPlayListener[WS.Secure](properties)
  }
}

trait WebSocketHandler extends WebSocket {
  def apply(authenticatedName: String): WebSocket
  def apply(authenticatedName: Option[String]): WebSocket
}
