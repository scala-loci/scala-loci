package loci
package communicator
package tcp

import java.io.IOException
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.net.Socket
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ArrayBuilder

private object TCPHandler {
  def handleConnection(
      socket: Socket,
      properties: TCP.Properties,
      connectionSetup: ConnectionSetup[TCP],
      connectionEstablished: Connection[TCP] => Unit) = {

    // socket streams

    val inputStream = new BufferedInputStream(socket.getInputStream)
    val outputStream = new BufferedOutputStream(socket.getOutputStream)


    // heartbeat

    val delay = properties.heartbeatDelay.toMillis
    val timeout = properties.heartbeatTimeout.toMillis.toInt
    val executor = Executors.newSingleThreadScheduledExecutor


    // control codes

    val head: Byte = 1
    val payload: Byte = 2
    val heartbeat: Byte = 6


    // connection interface

    val isOpen = new AtomicBoolean(true)
    val doClosed = Notifier[Unit]
    val doReceive = Notifier[MessageBuffer]

    val connection = new Connection[TCP] {
      val protocol = new TCP {
        val host = socket.getInetAddress.getHostName
        val port = socket.getPort
        val setup = connectionSetup
        val authenticated = false
        val encrypted = false
        val integrityProtected = false
      }

      val closed = doClosed.notification
      val receive = doReceive.notification

      def open = isOpen.get

      def send(data: MessageBuffer) =
        if (open) outputStream synchronized {
          try {
            val size = data.length
            outputStream write Array(
              head,
              (size >> 24).toByte,
              (size >> 16).toByte,
              (size >> 8).toByte,
              size.toByte,
              payload)
            outputStream write data.backingArray
            outputStream.flush
          }
          catch { case _: IOException => close }
        }

      def close() = isOpen synchronized {
        if (open) {
          def ignoreIOException(body: => Unit) =
            try body catch { case _: IOException => }

          executor.shutdown

          ignoreIOException { socket.shutdownOutput }
          ignoreIOException { while (inputStream.read != -1) { } }
          ignoreIOException { socket.close }

          isOpen set false
          doClosed()
        }
      }
    }

    connectionEstablished(connection)


    // heartbeat

    socket setSoTimeout timeout

    executor scheduleWithFixedDelay (new Runnable {
      def run = outputStream synchronized {
        try {
          outputStream write heartbeat
          outputStream.flush
        }
        catch { case _: IOException => connection.close }
      }
    }, delay, delay, TimeUnit.MILLISECONDS)


    // frame parsing

    def read = {
      val byte = inputStream.read
      if (byte == -1) throw new IOException("end of connection stream")
      else byte.toByte
    }

    val arrayBuilder = ArrayBuilder.make[Byte]

    try while (true) {
      read match {
        case `heartbeat` =>
          // heartbeat

        case `head` =>
          var size =
            ((read & 0xff) << 24) |
            ((read & 0xff) << 16) |
            ((read & 0xff) << 8) |
            (read & 0xff)

          if (read == payload && size >= 0) {
            arrayBuilder.clear
            arrayBuilder sizeHint size
            while (size > 0) {
              arrayBuilder += read
              size -= 1
            }

            doReceive(MessageBuffer wrapArray arrayBuilder.result)
          }
          else
            connection.close

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
