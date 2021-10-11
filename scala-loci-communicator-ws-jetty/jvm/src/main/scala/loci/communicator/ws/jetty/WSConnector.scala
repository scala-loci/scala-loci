package loci
package communicator
package ws.jetty

import org.eclipse.jetty.websocket.client.WebSocketClient

import java.net.URI
import scala.util.Failure

private class WSConnector[P <: WS : WSProtocolFactory](
  url: String, properties: WS.Properties)
    extends Connector[P] {

  protected def connect(connectionEstablished: Connected[P]): Unit = {
    val uri = URI.create(url)

    // Protocol

    val tls = uri.getScheme == "wss"
    val host = Some(uri.getHost)
    val port = Some(uri.getPort)

    val tryMakeProtocol = implicitly[WSProtocolFactory[P]].make(url, host, port, this, tls, tls, tls, request = None)

    if (tryMakeProtocol.isFailure) {
      connectionEstablished.set(Failure(tryMakeProtocol.failed.get))
      return
    }

    val prot = tryMakeProtocol.get


    // Socket

    val socket = new Socket[P](prot, properties, connectionEstablished.trySet(_), connectionEstablished.trySet(_))

    val client = new WebSocketClient()
    client.start()
    client.connect(socket, uri)
    socket.doClosed.notice.foreach(_ => client.stop())
  }
}

