package loci
package communicator
package ws.akka

import akka.util.ByteString
import play.api.http.websocket.{BinaryMessage, Message, TextMessage}

import scala.concurrent.Future
import scala.util.Try

private object WSPlayHandler {
  locally(WSPlayHandler)

  def handleWebSocket[P <: WS](
      ws: Future[P],
      properties: WS.Properties,
      connectionEstablished: Try[Connection[P]] => Unit) = {

    new WSAbstractHandler[Message] {
      def createTextMessage(data: String) = TextMessage(data)

      def createBinaryMessage(data: ByteString) = BinaryMessage(data)

      def processMessage(message: Message) = message match {
        case BinaryMessage(data) =>
          Some(Future successful data)
        case _ =>
          None
      }
    } handleWebSocket (ws, properties, connectionEstablished)
  }
}
