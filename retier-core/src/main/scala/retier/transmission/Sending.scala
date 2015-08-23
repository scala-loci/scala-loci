package retier
package transmission

import util.Notification

trait Sending[T] {
  val abstraction: AbstractionRef
  val closed: Notification[Unit] = abstraction.channel.closed

  def send(value: T): Unit
  def marshall(value: T, abstraction: AbstractionRef): String
}
