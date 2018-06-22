package loci
package transmitter

trait Endpoint[T, U] {
  val closed: Notification[Unit]
  def close(): Unit
  def send(value: T): Unit
  val receive: Notification[U]
}
