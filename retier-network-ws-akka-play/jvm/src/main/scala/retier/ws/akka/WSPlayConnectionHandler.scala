package retier
package ws.akka

import network.Connection
import play.api.http.websocket.Message
import play.api.http.websocket.TextMessage
import scala.concurrent.Future

private object WSPlayConnectionHandler {
  def handleWebsocket(
      protocolInfo: Future[WS],
      connectionEstablished: Connection => Unit,
      connectionFailed: Throwable => Unit) = {

    new WSAbstractConnectionHandler[Message] {
      def createMessage(data: String) = TextMessage(data)

      def processMessage(message: Message) = message match {
        case TextMessage(data) =>
          Future successful data

        case message =>
          Future failed new UnsupportedOperationException(
            s"Unsupported type of message: $message")
      }
    } handleWebsocket (protocolInfo, connectionEstablished, connectionFailed)
  }
}
