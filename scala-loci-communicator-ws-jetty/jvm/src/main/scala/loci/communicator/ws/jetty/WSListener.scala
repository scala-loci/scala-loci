package loci
package communicator
package ws.jetty

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.websocket.server.config.JettyWebSocketServletContainerInitializer
import org.eclipse.jetty.websocket.server.{JettyServerUpgradeRequest, JettyServerUpgradeResponse}

import scala.util.{Failure, Success, Try}

class WSListener[P <: WS : WSProtocolFactory](server: Server, contextPath: String, pathspec: String, properties: WS.Properties) extends Listener[P] {
  self =>

  override protected def startListening(connectionEstablished: Connected[P]): Try[Listening] = {
    val doClosed = Notice.Steady[Unit]
    val doReceive = Notice.Stream[MessageBuffer]
    val doConnect = Notice.Steady[Unit]

    val context = new ServletContextHandler(ServletContextHandler.SESSIONS)
    context.setContextPath(contextPath)
    server.setHandler(context)

    JettyWebSocketServletContainerInitializer.configure(context, (_, wsContainer) => {
      wsContainer.addMapping(pathspec, (_: JettyServerUpgradeRequest, _: JettyServerUpgradeResponse) => {
        val socket = new Socket[P](properties, doConnect, doReceive, doClosed, _ => {})

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

              val closed = doClosed.notice
              val receive = doReceive.notice

              override def open: Boolean = socket.getSession != null && socket.getSession.isOpen

              def send(data: MessageBuffer) = {
                if (socket.isConnected) {
                  socket.getRemote.sendBytes(data.asByteBuffer)
                }
              }

              def close() = socket.getSession.close()
            }

            doConnect.notice.foreach(_ => connectionEstablished.fire(Success(connection)))
        }

        socket
      })
    })

    Success(() => ())
  }
}
