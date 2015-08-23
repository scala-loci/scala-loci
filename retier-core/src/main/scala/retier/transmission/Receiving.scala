package retier
package transmission

import util.Notification
import scala.util.Try

trait Receiving[T] {
  val abstraction: AbstractionRef
  val closed: Notification[Unit] = abstraction.channel.closed
  def close(): Unit = abstraction.channel.close

  val receive: Notification[T]
  def unmarshall(value: String, abstraction: AbstractionRef): Try[T]
}
