package loci
package communicator
package ws.jetty

import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.websocket.server.NativeWebSocketServletContainerInitializer.Configurator
import org.eclipse.jetty.websocket.server.{NativeWebSocketConfiguration, NativeWebSocketServletContainerInitializer, WebSocketUpgradeFilter}
import org.eclipse.jetty.websocket.servlet.{ServletUpgradeRequest, ServletUpgradeResponse, WebSocketCreator}

import javax.servlet.ServletContext
import scala.util.{Failure, Success, Try}

class WSListener[P <: WS: WSProtocolFactory](
    context: ServletContextHandler,
    pathspec: String,
    properties: WS.Properties
) extends Listener[P] {
  self =>

  override protected def startListening(connectionEstablished: Connected[P]): Try[Listening] = {
    NativeWebSocketServletContainerInitializer.configure(
      context,
      new Configurator {
        def accept(context: ServletContext, wsContainer: NativeWebSocketConfiguration): Unit =
        wsContainer.addMapping(
          pathspec,
          new WebSocketCreator {
            def createWebSocket(request: ServletUpgradeRequest, repsonse: ServletUpgradeResponse): AnyRef = {
              val tryMakeProtocol = implicitly[WSProtocolFactory[P]].make(
                pathspec,
                None,
                None,
                self,
                authenticated = false,
                encrypted = false,
                integrityProtected = false,
                request = Some(request)
                )

              tryMakeProtocol match {
                case Failure(cause) =>
                  throw new IllegalStateException(s"creating protocol should not fail: $cause")

                case Success(prot) =>
                  new Socket[P](prot, properties, _ => {}, connectionEstablished.fire)
              }
            }
          })
      }
      )
    WebSocketUpgradeFilter.configure(context)

    Success(new Listening {
      def stopListening() = ()
    })
  }
}
