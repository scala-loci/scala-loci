package loci
package messaging

import transmitter.RemoteRef
import java.util.concurrent.ConcurrentHashMap

object Channels {
  trait Channel {
    val name: String
    val remote: RemoteRef
  }
}

class Channels[C <: Channels.Channel, R <: RemoteRef](
    createChannel: (String, R) => C,
    closeChannel: C => Unit) {

  private val channels = new ConcurrentHashMap[(String, R), C]

  def obtain(name: String, remote: R): C = {
    val channelId = (name, remote)
    val channel = createChannel(name, remote)
    if (remote.connected) {
      val obtainedChannel =
        Option(channels putIfAbsent (channelId, channel)) getOrElse channel

      if (!remote.connected)
        channels remove obtainedChannel

      obtainedChannel
    }
    else
      channel
  }

  def close(channel: C): Unit = {
    val channelId = (channel.name, channel.remote)
    Option(channels remove channelId) foreach closeChannel
  }

  def close(remote: R): Unit = {
    val iterator = channels.keySet.iterator
    while (iterator.hasNext)
      iterator.next match {
        case id @ (_, `remote`) =>
          Option(channels remove id) foreach closeChannel
        case _ =>
      }
  }

  def isOpen(channel: C): Boolean = {
    val channelId = (channel.name, channel.remote)
    channel == (channels get channelId)
  }
}
