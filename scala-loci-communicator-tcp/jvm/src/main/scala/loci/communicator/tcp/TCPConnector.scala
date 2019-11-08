package loci
package communicator
package tcp

import scala.util.Success
import scala.util.Failure
import scala.util.control.NonFatal
import java.net.Socket

private class TCPConnector(
  host: String, port: Int, properties: TCP.Properties)
    extends Connector[TCP] {

  protected def connect(connectionEstablished: Connected[TCP]): Unit = {
    new Thread() {
      override def run =
        try TCPHandler handleConnection (
          new Socket(host, port), properties, TCPConnector.this, { connection =>
            connectionEstablished set Success(connection)
          })
        catch {
          case NonFatal(exception) =>
            connectionEstablished set Failure(exception)
        }
    }.start    
  }
}
