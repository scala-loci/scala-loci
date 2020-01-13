package loci
package registry

import java.util.concurrent.ConcurrentHashMap

import messaging.Message
import messaging.Channels
import communicator.Connector
import communicator.Listener
import transmitter.RemoteAccessException
import transmitter.RemoteRef

import scala.collection.mutable.ListBuffer
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.concurrent.Promise
import scala.concurrent.Future

object Registry {
  case object Message {
    implicit val message =
      messaging.Message.Method(Message -> "Loci/Registry")

    final val Request = "Request"
    final val Response = "Response"
    final val Failure = "Failure"
    final val Channel = "Channel"
    final val Close = "Close"
  }

  private final case class Channel(
      name: String, anchor: String, remote: RemoteRef, registry: Registry)
        extends transmitter.Channel with Channels.Channel {
    val doReceive = Notice.Stream[MessageBuffer]
    val doClosed = Notice.Steady[Unit]

    val receive = doReceive.notice
    val closed = doClosed.notice

    def send(message: MessageBuffer) = registry send (this, message)
    def close() = registry.channels close (this, notifyRemote = true)
    def open = registry.channels isOpen this
  }

  private final case class AbstractionRef(name: String, remote: RemoteRef,
    channelName: String, channelAnchor: String, registry: Registry)
      extends transmitter.AbstractionRef {
    def derive(name: String) =
      AbstractionRef(this.name, remote, s"$channelName:$name", channelAnchor, registry)

    lazy val channel = registry.channels obtain (channelName, channelAnchor, remote)

    override def toString: String = s"$name#[channel:$channelName]$remote"
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

  private val bindings = new Bindings(request, respond)

  private val channelMessages =
    new ConcurrentHashMap[String, ListBuffer[Message[Registry.Message.type]]]

  val remoteJoined: Notice.Stream[RemoteRef] = connections.remoteJoined

  val remoteLeft: Notice.Stream[RemoteRef] = connections.remoteLeft

  def remotes: List[RemoteRef] = connections.remotes

  def running: Boolean = !connections.isTerminated

  def terminate(): Unit = connections.terminate


  connections.remoteJoined foreach { remote =>
    logging.trace(s"remote joined: $remote")
  }

  connections.remoteLeft foreach { remote =>
    logging.trace(s"remote left: $remote")
  }


  private def createChannel(name: String, anchorDefault: String, remote: RemoteRef) =
    Registry.Channel(name, anchorDefault, remote, this)

  private def closeChannel(channel: Registry.Channel, notifyRemote: Boolean) = {
    logging.trace(s"closing channel ${channel.name}")

    if (notifyRemote)
      bufferedSend(
        channel,
        Message(
          Registry.Message,
          Map(Registry.Message.Close -> Seq(channel.name)),
          MessageBuffer.empty))

    channel.doClosed.set()
  }


  private def send(channel: Registry.Channel, message: MessageBuffer) =
    if (channel.open)
      bufferedSend(
        channel,
        Message(
          Registry.Message,
          Map(Registry.Message.Channel -> Seq(channel.name)),
          message))

  private def bufferedSend(channel: Registry.Channel, message: Message[Registry.Message.type]): Unit = {
    val queued = Option(channelMessages get channel.anchor) exists { messages =>
      messages synchronized {
        val queued = channelMessages containsKey channel.anchor
        if (queued)
          messages += message
        queued
      }
    }

    if (!queued) {
      logging.trace(s"send update for channel ${channel.name} to ${channel.remote}: ${message.payload}")
      connections send (channel.remote, message)
    }
  }

  connections.remoteLeft foreach { remote =>
    channels close remote
    bindings.channelsClosed
  }

  connections.run

  connections.receive foreach { remoteMessage =>
    val (remote, Message(_, properties, message)) = remoteMessage
    (properties get Registry.Message.Request,
     properties get Registry.Message.Response,
     properties get Registry.Message.Failure,
     properties get Registry.Message.Channel,
     properties get Registry.Message.Close) match {
      case (Some(Seq(name)), None, None, Some(Seq(channelName)), None) =>
        channelMessages.put(channelName, ListBuffer.empty)
        bindings processRequest (
          message, name, Registry.AbstractionRef(name, remote, channelName, this))

      case (None, Some(Seq(name)), None, Some(Seq(channelName)), None) =>
        logging.debug(s"received response upon remote access for $channelName from $remote: $message")
        bindings processResponse (
          Success(message),
          name,
          Registry.AbstractionRef(name, remote, channelName, this))

      case (None, None, Some(Seq(name)), Some(Seq(channelName)), None) =>
        val exception = RemoteAccessException.deserialize(message.decodeString)
        logging.debug("received exception upon remote access", exception)
        bindings processResponse (
          Failure(exception),
          name,
          Registry.AbstractionRef(name, remote, channelName, this))

      case (None, None, None, Some(Seq(channelName)), None) =>
        channels get (channelName, remote) match {
          case None =>
            logging.warn(s"unprocessed message for channel $channelName from $remote: $message")
          case Some(channel) =>
            logging.trace(s"received update for channel $channelName from $remote: $message")
            channel.doReceive fire message
        }

      case (None, None, None, None, Some(Seq(channelName))) =>
        logging.trace(s"received update for channel $channelName from $remote: $message")
        channels get (channelName, remote) foreach { channels close (_, notifyRemote = false) }

      case _ =>
        logging.warn(s"unprocessed message from $remote: $message")
    }
  }

  private def request(abstraction: Registry.AbstractionRef, message: MessageBuffer) =
    send(Registry.Message.Request, abstraction, message)

  private def respond(abstraction: Registry.AbstractionRef, message: Try[MessageBuffer]) = {
    message match {
      case Success(message) =>
        send(
          Registry.Message.Response,
          abstraction,
          message)
      case Failure(exception) =>
        send(
          Registry.Message.Failure,
          abstraction,
          MessageBuffer.encodeString(RemoteAccessException.serialize(exception)))
    }

    Option(channelMessages get abstraction.channelAnchor) foreach { messages =>
      messages synchronized {
        messages foreach { message =>
          logging.trace(s"send update for channel ${abstraction.channel} to ${abstraction.remote}: ${message.payload}")
          connections send (abstraction.remote, message)
        }
        messages.clear
        channelMessages.remove(abstraction.channelAnchor)
      }
    }
  }

  private def send(method: String, abstraction: Registry.AbstractionRef, message: MessageBuffer) =
    connections send (
      abstraction.remote,
      Message(
        Registry.Message,
        Map(
          method -> Seq(abstraction.name),
          Registry.Message.Channel -> Seq(abstraction.channel.name)),
        message))


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

  def listen(listener: Listener[Connections.Protocol]): Try[Unit] =
    listenWithCallback(listener) { _ => }

  def listenWithCallback(listener: Listener[Connections.Protocol])(
      handler: Try[RemoteRef] => Unit): Try[Unit] =
    connections.listen(listener)(handler)
}
