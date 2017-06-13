package loci
package communicator
package tcp

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.util.control.NonFatal
import java.io.IOException
import java.net.Socket

private class TCPConnector(
  host: String, port: Int, properties: TCP.Properties)
    extends Connector[TCP] {

  protected def connect(handler: Handler[TCP]): Unit = {
    new Thread() {
      override def run =
        try TCPHandler handleConnection (
          new Socket(host, port), properties, TCPConnector.this, { connection =>
            handler notify Success(connection)
          })
        catch {
          case NonFatal(exception) =>
            handler notify Failure(exception)
        }
    }.start    
  }
}
