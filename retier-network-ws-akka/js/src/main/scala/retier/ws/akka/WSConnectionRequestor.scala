package retier
package ws.akka

import network.Connection
import network.ConnectionRequestor
import util.Notifier
import org.scalajs.dom
import scala.concurrent.Promise
import scala.scalajs.js.timers._
import scala.concurrent.duration._

private class WSConnectionRequestor(url: String) extends ConnectionRequestor {
  def request = {
    val promise = Promise[Connection]

    val socket = new dom.WebSocket(url)

    socket.onopen = { _: dom.Event =>
      // keep alive

      val delay = 20.seconds
      val timeout = 40.seconds

      val resetTimeout = Notifier[Unit]
      val resetInterval = Notifier[Unit]


      // connection interface

      val doClosed = Notifier[Unit]
      val doReceive = Notifier[String]

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

      val connection = new Connection {
        val protocol = WS.createProtocolInfo(url, host, port,
          WSConnectionRequestor.this,
          isEncrypted = tls, isProtected = tls, isAuthenticated = tls, None)
        val closed = doClosed.notification
        val receive = doReceive.notification

        def isOpen = socket.readyState == dom.WebSocket.OPEN
        def send(data: String) = {
          socket send ("#" + data)
          resetInterval()
        }
        def close() = socket.close()
      }

      promise success connection


      // keep alive

      var timeoutHandle: SetTimeoutHandle = null
      var intervalHandle: SetIntervalHandle = null

      resetTimeout.notification += { _ =>
        if (timeoutHandle != null)
          clearTimeout(timeoutHandle)
        timeoutHandle = setTimeout(timeout) { connection.close }
      }

      resetInterval.notification += { _ =>
        if (intervalHandle != null)
          clearInterval(intervalHandle)
        intervalHandle = setInterval(delay) { socket send "!" }
      }

      resetTimeout()
      resetInterval()


      // socket listeners

      socket.onclose = { event: dom.CloseEvent =>
        promise tryFailure new RemoteConnectionException(
          "connection closed: " + event.reason)
        clearInterval(intervalHandle)
        clearTimeout(timeoutHandle)
        doClosed()
      }

      socket.onerror = { event: dom.ErrorEvent =>
        promise tryFailure new RemoteConnectionException(
          "connection error: " + event.message)
        socket.close()
      }

      socket.onmessage = { event: dom.MessageEvent =>
        val data = event.data.toString
        if (data startsWith "#")
          doReceive(data substring 1)
        resetTimeout()
      }
    }

    promise.future
  }
}
