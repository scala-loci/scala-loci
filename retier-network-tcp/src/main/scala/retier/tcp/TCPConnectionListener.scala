package retier
package tcp

import network.ConnectionListener
import java.io.IOException
import java.net.InetAddress
import java.net.ServerSocket
import java.net.SocketException
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService

private class TCPConnectionListener(port: Int, interface: String)
    extends ConnectionListener {
  val lock = new Object
  @volatile private var serverSocket: ServerSocket = _
  @volatile private var executorService: ExecutorService = _

  def start() = lock synchronized {
    if (serverSocket == null) {
      val socket = new ServerSocket(port, 0, InetAddress.getByName(interface))
      val executor = Executors.newCachedThreadPool

      serverSocket = socket
      executorService = executor

      new Thread() {
        override def run =
          try {
            while (true) {
              val connection = socket.accept
              if (connection != null)
                executor execute new Runnable {
                  def run = TCPConnectionHandler handleConnection (
                    connection,
                    { doConnectionEstablished(_) })
                }
            }
          }
          catch { case _: SocketException => TCPConnectionListener.this.stop }
      }.start
    }
  }

  def stop() = lock synchronized {
    if (serverSocket != null) {
      try serverSocket.close
      catch { case _: IOException => }

      executorService.shutdown

      executorService = null
      serverSocket = null
    }
  }
}
