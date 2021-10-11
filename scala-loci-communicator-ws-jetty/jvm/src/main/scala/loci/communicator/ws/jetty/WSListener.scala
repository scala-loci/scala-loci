package loci
package communicator
package ws.jetty

import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.websocket.server.{NativeWebSocketServletContainerInitializer, WebSocketUpgradeFilter}
import org.eclipse.jetty.websocket.servlet.{ServletUpgradeRequest, ServletUpgradeResponse, WebSocketCreator}

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
      (_, wsContainer) => {
        wsContainer.addMapping(
          pathspec,
          new WebSocketCreator {
            override def createWebSocket(request: ServletUpgradeRequest, repsonse: ServletUpgradeResponse): AnyRef = {
              val tryMakeProtocol = implicitly[WSProtocolFactory[P]].make(
                pathspec,
                None,
                None,
                self,
                authenticated = false,
                encrypted = false,
                integrityProtected = false,
                request = Some(request),
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

    Success(() => ())
  }
}
