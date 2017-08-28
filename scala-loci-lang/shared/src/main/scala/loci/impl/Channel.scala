package loci
package impl

import Channel._
import RemoteRef._

private final case class ChannelImpl(name: String, remote: RemoteRef,
    system: System) extends Channel {
  val doReceive = Notifier[MessageBuffer]
  val doClosed = Notifier[Unit]

  val receive = doReceive.notification
  val closed = doClosed.notification

  def send(message: MessageBuffer) =
    system.sendMessage(this, message)
  def close() = system.closeChannel(this)
  def open = system.isChannelOpen(this)
}

object Channel {
  type Channel = transmitter.Channel

  private[impl] def create(name: String, remote: RemoteRef, system: System)
      : Channel =
    ChannelImpl(name, remote, system)

  implicit class ChannelOps(channel: Channel) {
    def receive(message: MessageBuffer): Unit = channel match {
      case channel @ ChannelImpl(_, _, _) =>
        channel.doReceive(message)
      case _ => throwLociImplementationError(channel)
    }

    def closed(): Unit = channel match {
      case channel @ ChannelImpl(_, _, _) =>
        channel.doClosed(())
      case _ => throwLociImplementationError(channel)
    }

    def name: String = channel match {
      case ChannelImpl(name, _, _) => name
      case _ => throwLociImplementationError(channel)
    }

    def remote: RemoteRef = channel match {
      case ChannelImpl(_, remote, _) => remote
      case _ => throwLociImplementationError(channel)
    }

    private def throwLociImplementationError(ref: Any) =
      throw new LociImplementationError(
        s"invalid channel implementation: ${className(ref)}")
  }
}
