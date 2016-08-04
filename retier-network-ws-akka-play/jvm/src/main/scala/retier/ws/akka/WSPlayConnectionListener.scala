package retier
package ws.akka

import network.ConnectionListener
import play.api.mvc.WebSocket
import play.api.mvc.RequestHeader
import play.api.mvc.Security.AuthenticatedRequest
import java.net.URI
import scala.concurrent.Future

private object WSPlayConnectionListener {
  def apply() = new ConnectionListener with WebSocketHandler {
    private def webSocket(authenticated: Boolean) = WebSocket accept { request =>
      val uri = new URI("dummy://" + request.host)
      val host = uri.getHost
      val port = uri.getPort

      WSPlayConnectionHandler handleWebsocket (
        Future successful
          WS.createProtocolInfo(
            request.uri,
            Option(host),
            if (port < 0) None else Some(port),
            this, request.secure, request.secure, authenticated),
        { doConnectionEstablished(_) }, Function const { })
    }

    def apply(authenticated: Boolean) = webSocket(authenticated)
    def apply(request: RequestHeader) = webSocket(
      authenticated = request.isInstanceOf[AuthenticatedRequest[_, _]])(request)
    def start() = { }
    def stop() = { }
  }
}
