package loci
package transmitter

trait Channel {
  val receive: Notification[MessageBuffer]
  val closed: Notification[Unit]

  def send(message: MessageBuffer): Unit
  def close(): Unit
  def open: Boolean
}
