package retier
package transmission

import util.Notification
import scala.util.Try

trait Channel {
  val receive: Notification[(String, String)]
  val closed: Notification[Unit]

  def send(messageType: String, payload: String): Unit
  def close(): Unit
}
