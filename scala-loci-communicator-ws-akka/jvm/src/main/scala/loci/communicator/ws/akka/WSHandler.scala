package loci
package communicator
package ws.akka

import akka.stream.Materializer
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import akka.util.ByteString
import akka.http.scaladsl.model.ws.Message
import akka.http.scaladsl.model.ws.TextMessage
import akka.http.scaladsl.model.ws.BinaryMessage
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.Queue
import java.util.concurrent.atomic.AtomicBoolean

private object WSHandler {
  locally(WSHandler)

  def handleWebSocket[P <: WS](
      ws: Future[P],
      properties: WS.Properties,
      connectionEstablished: Try[Connection[P]] => Unit)(implicit
      materializer: Materializer) = {

    new WSAbstractHandler[Message] {
      def createTextMessage(data: String) = TextMessage(data)

      def createBinaryMessage(data: ByteString) = BinaryMessage(data)

      def processMessage(message: Message) = message match {
        case BinaryMessage.Strict(data) =>
          Some(Future successful data)
        case BinaryMessage.Streamed(dataStream) =>
          Some(dataStream.runFold(ByteString()) { _ ++ _ })
        case _ =>
          None
      }
    } handleWebSocket (ws, properties, connectionEstablished)
  }
}

private abstract class WSAbstractHandler[M] {
  def createTextMessage(data: String): M

  def createBinaryMessage(data: ByteString): M

  def processMessage(message: M): Option[Future[ByteString]]

  def handleWebSocket[P <: WS](
      ws: Future[P],
      properties: WS.Properties,
      connectionEstablished: Try[Connection[P]] => Unit) = {

    // connection interface

    val promises = Queue.empty[Promise[Option[(Unit, M)]]]
    val isOpen = new AtomicBoolean(true)
    val doClosed = Notifier[Unit]
    val doReceive = Notifier[MessageBuffer]

    def connectionOpen = isOpen.get

    def connectionSend(data: MessageBuffer) = promises synchronized {
      if (connectionOpen) {
        val message =
          Some(((), createBinaryMessage(ByteString(data.backingArray))))
        if (!promises.isEmpty && !promises.head.isCompleted)
          promises.dequeue success message
        else
          promises enqueue (Promise successful message)
      }
    }

    def connectionClose() = promises synchronized {
      if (connectionOpen) {
        isOpen set false
        promises foreach { _ trySuccess None }
        promises.clear
        doClosed()
      }
    }

    ws foreach { ws =>
      val connection = new Connection[P] {
        val protocol = ws

        val closed = doClosed.notification
        val receive = doReceive.notification

        def open = connectionOpen
        def send(data: MessageBuffer) = connectionSend(data)
        def close() = connectionClose
      }

      connectionEstablished(Success(connection))
    }


    // source for sending messages

    val source = Source.unfoldAsync(()) { _ =>
      promises synchronized {
        if (connectionOpen) {
          if (promises.isEmpty) {
            val promise = Promise[Option[(Unit, M)]]
            promises enqueue promise
            promise.future
          }
          else
            promises.dequeue.future
        }
        else
          Future successful None
      }
    }


    // sink for receiving messages

    val sink = Sink foreach[M] { message =>
      processMessage(message) foreach {
        _ onComplete {
          case Success(data) =>
            doReceive(MessageBuffer wrapArray data.toArray)
          case Failure(_) =>
            connectionClose
        }
      }
    }


    // flow

    val flow = Flow.fromSinkAndSourceMat(sink, source) { (future, _) =>
      future onComplete { _ => connectionClose }
    }

    def heartbeatMessage() = createTextMessage("\uD83D\uDC93")

    (Flow[M]
      idleTimeout properties.heartbeatTimeout
      via flow
      keepAlive (properties.heartbeatDelay, heartbeatMessage _))
  }
}
