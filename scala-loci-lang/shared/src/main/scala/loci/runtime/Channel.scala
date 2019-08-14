package loci
package runtime

case class Channel(name: String, anchor: String, remote: Remote.Reference, system: System)
    extends transmitter.Channel {

  val doReceive = Notifier[MessageBuffer]
  val doClosed = Notifier[Unit]

  val receive = doReceive.notification
  val closed = doClosed.notification

  def send(message: MessageBuffer) = system.sendMessage(this, message)
  def close() = system.closeChannel(this)
  def open = system.isChannelOpen(this)
}
