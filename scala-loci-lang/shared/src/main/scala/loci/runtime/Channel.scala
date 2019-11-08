package loci
package runtime

case class Channel(name: String, anchor: String, remote: Remote.Reference, system: System)
    extends transmitter.Channel {

  val doReceive = Notice.Stream[MessageBuffer]
  val doClosed = Notice.Steady[Unit]

  val receive = doReceive.notice
  val closed = doClosed.notice

  def send(message: MessageBuffer) = system.sendMessage(this, message)
  def close() = system.closeChannel(this, notifyRemote = true)
  def open = system.isChannelOpen(this)
}
