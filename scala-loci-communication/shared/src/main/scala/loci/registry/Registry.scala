package loci
package registry

import java.util.concurrent.ConcurrentHashMap

import messaging.Message
import messaging.Channels
import communicator.Connector
import communicator.Listener
import transmitter.RemoteRef

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.concurrent.Promise
import scala.concurrent.Future

object Registry {
  case object Message {
    implicit val message =
      messaging.Message.Method(Message -> "Loci/Registry")

    final val Request = "Request"
    final val Response = "Response"
    final val Channel = "Channel"
  }

  private final case class Channel(
      name: String, anchor: String, remote: RemoteRef, registry: Registry)
        extends transmitter.Channel with Channels.Channel {
    val doReceive = Notifier[MessageBuffer]
    val doClosed = Notifier[Unit]

    val receive = doReceive.notification
    val closed = doClosed.notification

    def send(message: MessageBuffer) = registry send (this, message)
    def close() = registry.channels close this
    def open = registry.channels isOpen this
  }

  private final case class AbstractionRef(name: String, remote: RemoteRef,
    channelName: String, channelAnchor: String, registry: Registry)
      extends transmitter.AbstractionRef {
    def derive(name: String) =
      AbstractionRef(this.name, remote, s"$channelName:$name", channelAnchor, registry)

    lazy val channel = registry.channels obtain (channelName, channelAnchor, remote)
  }

  private object AbstractionRef {
    def apply(name: String, remote: RemoteRef,
        channelName: String, registry: Registry): AbstractionRef =
      AbstractionRef(name, remote, channelName, channelName, registry)
  }
}

class Registry {
  private val connections = new Connections[Registry.Message.type]

  private val channels = new Channels(createChannel, closeChannel)

  private val bindings = new Bindings(
    handleLookup(Registry.Message.Request),
    handleLookup(Registry.Message.Response))

  private val channelMessages =
    new ConcurrentHashMap[String, ListBuffer[Message[Registry.Message.type]]]

  val remoteJoined: Notification[RemoteRef] = connections.remoteJoined

  val remoteLeft: Notification[RemoteRef] = connections.remoteLeft

  def remotes: List[RemoteRef] = connections.remotes

  def running: Boolean = !connections.isTerminated

  def terminate(): Unit = connections.terminate


  private def createChannel(name: String, anchorDefault: String, remote: RemoteRef) =
    Registry.Channel(name, anchorDefault, remote, this)

  private def closeChannel(channel: Registry.Channel) =
    channel.doClosed()


  private def send(channel: Registry.Channel, message: MessageBuffer) =
    if (channel.open) {
      val channelMessage = Message(
        Registry.Message,
        Map(Registry.Message.Channel -> Seq(channel.name)),
        message)

      val queued = Option(channelMessages get channel.anchor) exists { messages =>
        messages synchronized {
          val queued = channelMessages containsKey channel.anchor
          if (queued)
            messages += channelMessage
          queued
        }
      }

      if (!queued)
        connections send (channel.remote, channelMessage)
    }

  connections.remoteLeft notify { remote =>
    channels close remote
    bindings.channelsClosed
  }

  connections.run

  connections.receive notify { remoteMessage =>
    val (remote, Message(_, properties, message)) = remoteMessage
    (properties get Registry.Message.Request,
     properties get Registry.Message.Response,
     properties get Registry.Message.Channel) match {
      case (Some(Seq(name)), None, Some(Seq(channelName))) =>
        channelMessages.put(channelName, ListBuffer.empty)
        bindings processRequest (
          message, name, Registry.AbstractionRef(name, remote, channelName, this))

      case (None, Some(Seq(name)), Some(Seq(channelName))) =>
        bindings processResponse (
          message, name, Registry.AbstractionRef(name, remote, channelName, this))

      case (None, None, Some(Seq(channelName))) =>
        channels get (channelName, remote) foreach { _ doReceive message }

      case _ =>
        // unknown message
    }
  }

  private def handleLookup(method: String)(
      abstraction: Registry.AbstractionRef, message: MessageBuffer) = {
    connections send (
      abstraction.remote,
      Message(
        Registry.Message,
        Map(
          method -> Seq(abstraction.name),
          Registry.Message.Channel -> Seq(abstraction.channel.name)),
        message))

    Option(channelMessages get abstraction.channelAnchor) foreach { messages =>
      messages synchronized {
        messages foreach { connections send (abstraction.remote, _) }
        messages.clear
        channelMessages.remove(abstraction.channelAnchor)
      }
    }
  }


  def bindValue[T](name: String)(function: T)(
      implicit builder: BindingBuilder.Value[T]): Unit =
    bind(builder(name))(function)

  def bind[T](name: String)(function: T)(
      implicit builder: BindingBuilder[T]): Unit =
    bind(builder(name))(function)

  def bind[T](binding: Binding[T])(function: T): Unit =
    bindings.bind(binding)(_ => function)

  def bindValuePerRemote[T](name: String)(function: RemoteRef => T)(
      implicit builder: BindingBuilder.Value[T]): Unit =
    bindPerRemote(builder(name))(function)

  def bindPerRemote[T](name: String)(function: RemoteRef => T)(
      implicit builder: BindingBuilder[T]): Unit =
    bindPerRemote(builder(name))(function)

  def bindPerRemote[T](binding: Binding[T])(function: RemoteRef => T): Unit =
    bindings.bind(binding)(function)

  def lookupValue[T](name: String, remote: RemoteRef)(
      implicit builder: BindingBuilder.Value[T]): builder.RemoteCall =
    lookup(builder(name), remote)

  def lookup[T](name: String, remote: RemoteRef)(
      implicit builder: BindingBuilder[T]): builder.RemoteCall =
    lookup(builder(name), remote)

  def lookup[T](binding: Binding[T], remote: RemoteRef): binding.RemoteCall =
    bindings.lookup(
      binding,
      Registry.AbstractionRef(
        binding.name,
        remote,
        java.util.UUID.randomUUID.toString,
        this))


  def connect(connector: Connector[Connections.Protocol]): Future[RemoteRef] = {
    val promise = Promise[RemoteRef]
    connectWithCallback(connector) { promise complete _ }
    promise.future
  }

  def connectWithCallback(connector: Connector[Connections.Protocol])(
      handler: Try[RemoteRef] => Unit): Unit =
    connections.connect(connector)(handler)

  def listen(listener: Listener[Connections.Protocol]): Unit =
    listenWithCallback(listener) { _ => }

  def listenWithCallback(listener: Listener[Connections.Protocol])(
      handler: Try[RemoteRef] => Unit): Unit =
    connections.listen(listener)(handler)
}
