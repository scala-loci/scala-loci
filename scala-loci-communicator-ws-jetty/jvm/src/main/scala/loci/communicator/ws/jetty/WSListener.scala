package loci
package communicator
package ws.jetty

import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.websocket.server.NativeWebSocketServletContainerInitializer.Configurator
import org.eclipse.jetty.websocket.server.{NativeWebSocketConfiguration, NativeWebSocketServletContainerInitializer, WebSocketUpgradeFilter}
import org.eclipse.jetty.websocket.servlet.{ServletUpgradeRequest, ServletUpgradeResponse, WebSocketCreator}

import javax.servlet.ServletContext
import scala.util.{Failure, Success, Try}

private class WSListener[P <: WS: WSProtocolFactory](
    context: ServletContextHandler,
    pathspec: String,
    properties: WS.Properties) extends Listener[P] {
  self =>

  protected def startListening(connectionEstablished: Connected[P]): Try[Listening] = {
    NativeWebSocketServletContainerInitializer.configure(
      context,
      new Configurator {
        def accept(context: ServletContext, wsContainer: NativeWebSocketConfiguration): Unit =
          wsContainer.addMapping(
            pathspec,
            new WebSocketCreator {
              def createWebSocket(request: ServletUpgradeRequest, repsonse: ServletUpgradeResponse): AnyRef = {
                val uri = request.getRequestURI
                val tls = uri.getScheme == "wss"

                implicitly[WSProtocolFactory[P]].make(
                    request.getRequestURI.toString, Some(uri.getHost), Some(uri.getPort),
                    self, tls, tls, tls, Some(request)) match {
                  case Failure(exception) =>
                    connectionEstablished.fire(Failure(exception))
                    null

                  case Success(ws) =>
                    new Socket[P](ws, properties)(connectionEstablished.fire, Function.const(()))
                }
              }
            })
      })

    WebSocketUpgradeFilter.configure(context)

    Success(new Listening {
      def stopListening() = ()
    })
  }
}
