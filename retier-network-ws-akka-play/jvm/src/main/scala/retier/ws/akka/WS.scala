package retier
package ws.akka

import network.ConnectionListener
import play.api.mvc.WebSocket

object WebSocketHandler {
  def apply(): ConnectionListener with WebSocketHandler =
    WSPlayConnectionListener()
}

trait WebSocketHandler extends WebSocket {
  def apply(v: Boolean): WebSocket
}
