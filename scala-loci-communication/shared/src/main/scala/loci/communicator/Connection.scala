package loci
package communicator

trait Connection[+P <: ProtocolCommon] {
  val receive: Notification[MessageBuffer]
  val closed: Notification[Unit]

  def send(message: MessageBuffer): Unit
  def close(): Unit
  def open: Boolean

  val protocol: P
}
