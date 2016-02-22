package retier
package ws.akka

import network.Connection
import util.Notifier
import contexts.Immediate.Implicits.global
import akka.stream.Materializer
import akka.stream.stage.PushStage
import akka.stream.stage.Context
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import akka.http.scaladsl.model.ws.Message
import akka.http.scaladsl.model.ws.TextMessage
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.collection.mutable.Queue
import java.lang.StringBuilder
import java.util.concurrent.atomic.AtomicBoolean

private object WSConnectionHandler {
  def handleWebsocket(
      protocolInfo: Future[WS],
      connectionEstablished: Connection => Unit,
      connectionFailed: Throwable => Unit)
    (implicit
      materializer: Materializer) = {

    // keep alive

    val delay = 20.seconds
    val timeout = 40.seconds


    // connection interface

    val promises = Queue.empty[Promise[Option[(Unit, Message)]]]
    val open = new AtomicBoolean(true)
    val doClosed = Notifier[Unit]
    val doReceive = Notifier[String]

    def connectionOpen = open.get

    def connectionSend(data: String) = promises synchronized {
      if (connectionOpen) {
        val message = Some(((), TextMessage("#" + data)))
        if (!promises.isEmpty && !promises.head.isCompleted)
          promises.dequeue success message
        else
          promises enqueue (Promise successful message)
      }
    }

    def connectionClose() = promises synchronized {
      if (connectionOpen) {
        open set false
        promises foreach { _ trySuccess None }
        promises.clear
        doClosed()
      }
    }

    protocolInfo onSuccess { case protocolInfo =>
      promises synchronized {
        val connection = new Connection {
          val protocol = protocolInfo
          val closed = doClosed.notification
          val receive = doReceive.notification

          def isOpen = connectionOpen
          def send(data: String) = connectionSend(data)
          def close() = connectionClose
        }

        connectionEstablished(connection)
      }
    }


    // source, sink and flow

    val source = Source.unfoldAsync(()) { _ =>
      promises synchronized {
        if (connectionOpen) {
          if (promises.isEmpty) {
            val promise = Promise[Option[(Unit, Message)]]
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

    val sink = Sink foreach[Message] {
      case TextMessage.Strict(data) =>
        if (data startsWith "#")
          doReceive(data substring 1)

      case message: TextMessage =>
        message.textStream.runFold(new StringBuilder) {
          case (builder, data) =>
            builder append data
        } onSuccess {
          case builder =>
            val data = builder.toString
            if (data startsWith "#")
              doReceive(data substring 1)
        }

      case _ =>
        connectionClose
    }


    // flow

    val flow = Flow.fromSinkAndSourceMat(sink, source) { (future, _) =>
      future onComplete { _ => connectionClose }
    }

    def closeConnectionOnFailure[T]() = new PushStage[T, T] {
      def onPush(elem: T, ctx: Context[T]) = ctx push elem

      override def onUpstreamFailure(cause: Throwable, ctx: Context[T]) = {
        connectionClose
        connectionFailed(cause)
        super.onUpstreamFailure(cause, ctx)
      }
    }

    def keepAliveMessage() = TextMessage("!")

    (Flow[Message]
      idleTimeout timeout
      transform closeConnectionOnFailure
      via flow
      keepAlive (delay, keepAliveMessage))
  }
}
