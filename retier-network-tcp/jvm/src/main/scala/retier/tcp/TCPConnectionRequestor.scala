package retier
package tcp

import network.Connection
import network.ConnectionRequestor
import scala.concurrent.Promise
import java.io.IOException
import java.net.Socket

private class TCPConnectionRequestor(host: String, port: Int)
    extends ConnectionRequestor {
  def request = {
    val promise = Promise[Connection]

    new Thread() {
      override def run =
        try TCPConnectionHandler handleConnection (
          new Socket(host, port), { promise success _ })
        catch {
          case exception: IOException =>
            promise failure exception
        }
    }.start

    promise.future
  }
}
