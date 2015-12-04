package retier
package transmission

import util.Notification

trait Receiving[T] {
  val abstraction: AbstractionRef
  val closed: Notification[Unit] = abstraction.channel.closed
  def close(): Unit = abstraction.channel.close
  val receive: Notification[T]
}
