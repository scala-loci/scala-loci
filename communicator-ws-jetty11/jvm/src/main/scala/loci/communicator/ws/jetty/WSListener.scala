package loci
package communicator
package ws.jetty

import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.websocket.server.{JettyServerUpgradeRequest, JettyServerUpgradeResponse, JettyWebSocketCreator, JettyWebSocketServerContainer}
import org.eclipse.jetty.websocket.server.config.JettyWebSocketServletContainerInitializer.Configurator
import org.eclipse.jetty.websocket.servlet.WebSocketUpgradeFilter
import org.eclipse.jetty.websocket.core.server.ServerUpgradeRequest
import jakarta.servlet.ServletContext
import org.eclipse.jetty.websocket.server.config.JettyWebSocketServletContainerInitializer

import scala.util.{Failure, Success, Try}

private class WSListener[P <: WS: WSProtocolFactory](
    context: ServletContextHandler,
    pathspec: String,
    properties: WS.Properties) extends Listener[P] {
  self =>

  protected def startListening(connectionEstablished: Connected[P]): Try[Listening] = {
    JettyWebSocketServletContainerInitializer.configure(
      context,
      new Configurator {
        def accept(context: ServletContext, wsContainer: JettyWebSocketServerContainer): Unit =
          wsContainer.addMapping(
            pathspec,
            new JettyWebSocketCreator {
              def createWebSocket(request: JettyServerUpgradeRequest, response: JettyServerUpgradeResponse): AnyRef = {
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

    //WebSocketUpgradeFilter.configure(context)

    Success(new Listening {
      def stopListening() = ()
    })
  }
}
