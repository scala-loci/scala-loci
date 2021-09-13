package loci
package communicator
package ws.jetty

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.websocket.server.NativeWebSocketServletContainerInitializer
import org.eclipse.jetty.websocket.servlet.{ServletUpgradeRequest, ServletUpgradeResponse, WebSocketCreator}

import scala.util.{Failure, Success, Try}

class WSListener[P <: WS : WSProtocolFactory](server: Server, contextPath: String, pathspec: String, properties: WS.Properties) extends Listener[P] {
  self =>

  override protected def startListening(connectionEstablished: Connected[P]): Try[Listening] = {
    val doClosed = Notice.Steady[Unit]
    val doReceive = Notice.Stream[MessageBuffer]

    val context = new ServletContextHandler(ServletContextHandler.SESSIONS)
    context.setContextPath(contextPath)
    server.setHandler(context)

    NativeWebSocketServletContainerInitializer.configure(context, (_, wsContainer) => {
      wsContainer.addMapping(pathspec, new WebSocketCreator {
        override def createWebSocket(req: ServletUpgradeRequest, resp: ServletUpgradeResponse): Socket[P] = {
          val socket = new Socket[P](properties, doReceive, doClosed, _ => {})

          val tryMakeProtocol = implicitly[WSProtocolFactory[P]].make(
            pathspec,
            None,
            None,
            self,
            authenticated = false,
            encrypted = false,
            integrityProtected = false
          )

          tryMakeProtocol match {
            case Failure(_) =>

            case Success(prot) =>
              val connection = new Connection[P] {
                val protocol = prot

                val closed  = doClosed.notice
                val receive = doReceive.notice

                override def open: Boolean = socket.getSession.isOpen

                def send(data: MessageBuffer) = {
                  if (socket.isConnected) {
                    socket.getRemote.sendBytes(data.asByteBuffer)
                  }
                }

                def close() = socket.getSession.close()
              }

              connectionEstablished.fire(Success(connection))
          }

          socket
        }
      })
    })

    Success(() => ())
  }
}
