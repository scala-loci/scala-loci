package loci
package communicator
package ws.jetty

import org.eclipse.jetty.websocket.client.WebSocketClient

import java.net.URI
import scala.util.{Failure, Success}

private class WSConnector[P <: WS : WSProtocolFactory](
  url: String, properties: WS.Properties)
    extends Connector[P] {

  protected def connect(connectionEstablished: Connected[P]): Unit = {
    val uri = URI.create(url)

    val client = new WebSocketClient()

    try {
      client.start()

      // Socket

      val doClosed = Notice.Steady[Unit]
      val doReceive = Notice.Stream[MessageBuffer]

      val socket = new Socket[P](properties, doReceive, doClosed, connectionEstablished.trySet(_))

      val session = client.connect(socket, uri).get()

      // Protocol

      val tls = session.isSecure
      val host = Some(session.getRemoteAddress.getHostName)
      val port = Some(session.getRemoteAddress.getPort)

      val tryMakeProtocol = implicitly[WSProtocolFactory[P]].make(url, host, port, this, tls, tls, tls)

      if (tryMakeProtocol.isFailure) {
        connectionEstablished.set(Failure(tryMakeProtocol.failed.get))
        return
      }

      val prot = tryMakeProtocol.get

      // Connection interface

      val connection = new Connection[P] {
        val protocol = prot
        val closed = doClosed.notice
        val receive = doReceive.notice

        def open = session.isOpen
        def send(data: MessageBuffer) = {
          session.getRemote.sendBytes(data.asByteBuffer)
        }
        def close() = session.close()
      }

      connectionEstablished.set(Success(connection))

    } finally {
      client.stop()
    }
  }
}

