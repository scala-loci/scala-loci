package loci
package communicator
package ws.jetty

import org.eclipse.jetty.util.Callback
import org.eclipse.jetty.websocket.server.{ServerUpgradeResponse, ServerUpgradeRequest, WebSocketCreator, WebSocketUpgradeHandler}

import scala.util.{Failure, Success, Try}

private class WSListener[P <: WS: WSProtocolFactory](
    webSocketUpgradeHandler: WebSocketUpgradeHandler,
    pathspec: String,
    properties: WS.Properties) extends Listener[P] {
  self =>

  protected def startListening(connectionEstablished: Connected[P]): Try[Listening] = Try {
    webSocketUpgradeHandler.getServerWebSocketContainer.addMapping(
      pathspec,
      new WebSocketCreator {
        def createWebSocket(request: ServerUpgradeRequest, response: ServerUpgradeResponse, callback: Callback) = {
          val uri = request.getHttpURI
          val tls = uri.getScheme == "wss"

          implicitly[WSProtocolFactory[P]].make(
              request.getHttpURI.toString, Some(uri.getHost), Some(uri.getPort),
              self, tls, tls, tls, Some(request)) match {
            case Failure(exception) =>
              connectionEstablished.fire(Failure(exception))
              callback.failed(exception)
              null

            case Success(ws) =>
              new Socket[P](ws, properties)(connectionEstablished.fire, Function.const(()))
          }
        }
      })

    new Listening {
      def stopListening() = ()
    }
  }
}
