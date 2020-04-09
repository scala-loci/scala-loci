package loci
package communicator
package ws.akka

import java.util.concurrent.atomic.AtomicBoolean

import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.ByteString

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

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

    val promises = mutable.Queue.empty[Promise[Option[(Unit, M)]]]
    val isOpen = new AtomicBoolean(true)
    val doClosed = Notice.Steady[Unit]
    val doReceive = Notice.Stream[MessageBuffer]

    def connectionOpen = isOpen.get

    def connectionSend(data: MessageBuffer) = promises synchronized {
      if (connectionOpen) {
        val message =
          Some(((), createBinaryMessage(ByteString(data.backingArray))))
        if (promises.nonEmpty && !promises.head.isCompleted)
          promises.dequeue().success(message)
        else
          promises.enqueue(Promise successful message)
      }
    }

    def connectionClose() = promises synchronized {
      if (connectionOpen) {
        isOpen.set(false)
        promises foreach { _.trySuccess(None) }
        promises.clear()
        doClosed.set()
      }
    }

    ws foreach { ws =>
      val connection = new Connection[P] {
        val protocol = ws

        val closed = doClosed.notice
        val receive = doReceive.notice

        def open = connectionOpen
        def send(data: MessageBuffer) = connectionSend(data)
        def close() = connectionClose()
      }

      connectionEstablished(Success(connection))
    }


    // source for sending messages

    val source = Source.unfoldAsync(()) { _ =>
      promises synchronized {
        if (connectionOpen) {
          if (promises.isEmpty) {
            val promise = Promise[Option[(Unit, M)]]
            promises.enqueue(promise)
            promise.future
          }
          else
            promises.dequeue().future
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
            doReceive.fire(MessageBuffer wrapArray data.toArray)
          case Failure(_) =>
            connectionClose()
        }
      }
    }


    // flow

    val flow = Flow.fromSinkAndSourceMat(sink, source) { (future, _) =>
      future onComplete { _ => connectionClose() }
    }

    def heartbeatMessage() = createTextMessage("\uD83D\uDC93")

    Flow[M]
      .idleTimeout(properties.heartbeatTimeout)
      .via(flow)
      .keepAlive(properties.heartbeatDelay, heartbeatMessage _)
  }
}
