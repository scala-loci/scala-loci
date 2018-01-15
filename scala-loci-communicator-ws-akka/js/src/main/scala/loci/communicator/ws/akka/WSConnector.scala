package loci
package communicator
package ws.akka

import org.scalajs.dom
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.timers._

private class WSConnector[P <: WS: WSProtocolFactory](
  url: String, properties: WS.Properties)
    extends Connector[P] {

  protected def connect(handler: Handler[P]) = {

    val socket = new dom.WebSocket(url)

    socket.onopen = { _: dom.Event =>

      // protocol properties

      val (tls, host, port) = dom.document.createElement("a") match {
        case parser: dom.html.Anchor =>
          parser.href = url
          val tls = (parser.protocol compareToIgnoreCase "wss:") == 0
          val host = Some(parser.hostname)
          val port =
            try Some(parser.port.toInt)
            catch { case _: NumberFormatException => None }
          (tls, host, port)
        case _ =>
          (false, None, None)
      }

      implicitly[WSProtocolFactory[P]] make (url, host, port, this, tls, tls, tls) match {
        case Failure(exception) =>
          handler notify Failure(exception)

        case Success(ws) =>

          // heartbeat

          var timeoutHandle: SetTimeoutHandle = null
          var intervalHandle: SetIntervalHandle = null

          val resetTimeout = Notifier[Unit]
          val resetInterval = Notifier[Unit]


          // connection interface

          val doClosed = Notifier[Unit]
          val doReceive = Notifier[MessageBuffer]

          val connection = new Connection[P] {
            val protocol = ws
            val closed = doClosed.notification
            val receive = doReceive.notification

            def open = socket.readyState == dom.WebSocket.OPEN
            def send(data: MessageBuffer) = {
              socket send data.backingArrayBuffer
              resetInterval()
            }
            def close() = socket.close()
          }

          handler notify Success(connection)


          // heartbeat

          resetTimeout.notification notify { _ =>
            if (timeoutHandle != null)
              clearTimeout(timeoutHandle)
            timeoutHandle = setTimeout(properties.heartbeatTimeout) {
              connection.close
            }
          }

          resetInterval.notification notify { _ =>
            if (intervalHandle != null)
              clearInterval(intervalHandle)
            intervalHandle = setInterval(properties.heartbeatDelay) {
              socket send "\uD83D\uDC93"
            }
          }

          resetTimeout()
          resetInterval()


          // socket listeners

          socket.onmessage = { event: dom.MessageEvent =>
            event.data match {
              case data: ArrayBuffer =>
                doReceive(MessageBuffer wrapArrayBuffer data)

              case data: dom.Blob =>
                val reader = new dom.FileReader
                reader.onload = { event: dom.Event =>
                  doReceive(MessageBuffer wrapArrayBuffer
                    event.target.asInstanceOf[js.Dynamic]
                         .result.asInstanceOf[ArrayBuffer])
                }
                reader readAsArrayBuffer data

              case _ =>
            }
            resetTimeout()
          }

          socket.onclose = { event: dom.CloseEvent =>
            handler notify Failure(
              new ConnectionException("connection closed: " + event.reason))

            clearInterval(intervalHandle)
            clearTimeout(timeoutHandle)
            doClosed()
          }
      }
    }

    socket.onerror = { event: dom.Event =>
      handler notify Failure(
        new ConnectionException("connection closed: connection error"))

      socket.close()
    }
  }
}
