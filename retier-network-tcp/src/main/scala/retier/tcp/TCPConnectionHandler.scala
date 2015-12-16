package retier
package tcp

import network.Connection
import util.Notifier
import java.io.IOException
import java.io.OutputStreamWriter
import java.io.InputStreamReader
import java.io.BufferedReader
import java.net.Socket
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.TimeUnit

private object TCPConnectionHandler {
  def handleConnection(
      socket: Socket,
      connectionEstablished: Connection => Unit) = {

    // socket streams

    val reader = new BufferedReader(new InputStreamReader(
      socket.getInputStream, "UTF-8"))

    val writer = new OutputStreamWriter(
      socket.getOutputStream, "UTF-8")


    // keep alive

    val delay = 20000
    val timeout = 40000

    socket setSoTimeout timeout

    val executor = Executors.newSingleThreadScheduledExecutor
    executor scheduleWithFixedDelay (new Runnable {
      def run = writer synchronized {
        writer write ("!", 0, 1)
        writer.flush
      }
    }, delay, delay, TimeUnit.MILLISECONDS)


    // connection interface

    val open = new AtomicBoolean(true)
    val doClosed = Notifier[Unit]
    val doReceive = Notifier[String]

    val connection = new Connection {
      val protocol = TCP.createProtocolInfo(
        socket.getInetAddress.getHostName,
        socket.getPort)
      val closed = doClosed.notification
      val receive = doReceive.notification

      def isOpen = open.get

      def send(data: String) =
        if (isOpen) writer synchronized {
          val output = "[" + data.length + "]" + data
          writer write (output, 0, output.length)
          writer.flush
        }

      def close() = open synchronized {
        if (isOpen) {
          def ignoreIOException(body: => Unit) =
            try body catch { case _: IOException => }

          executor.shutdown

          ignoreIOException { socket.shutdownOutput }
          ignoreIOException { while (reader.read != -1) { } }
          ignoreIOException { socket.close }

          open set false
          doClosed()
        }
      }
    }

    connectionEstablished(connection)


    // frame parsing

    def read = {
      val ch = reader.read
      if (ch == -1) throw new IOException("end of connection stream")
      else ch.asInstanceOf[Char]
    }

    val builder = new StringBuilder

    try while (true) {
      read match {
        case '[' =>
          builder setLength 0
          var ch = read
          while (ch != ']') {
            builder append ch
            ch = read
          }

          val size = Integer parseInt builder.toString

          builder setLength 0
          for (_ <- 0 until size)
            builder append read

          doReceive(builder.toString)

        case '!' =>
          // keep alive

        case _ =>
          connection.close
      }
    }
    catch {
      case _: IOException =>
        connection.close
      case _: NumberFormatException =>
        connection.close
    }
  }
}
