package loci
package registry

import messaging.Message
import messaging.Channels
import communicator.Connector
import communicator.Listener
import transmitter.RemoteRef
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
      name: String, remote: RemoteRef, registry: Registry)
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
    channel: Channel, registry: Registry)
      extends transmitter.AbstractionRef {
    def derive(name: String) =
      AbstractionRef(
        this.name,
        remote,
        registry.channels obtain (channel.name + ":" + name, remote),
        registry)
  }
}

class Registry {
  private val connections = new Connections[Registry.Message.type]

  private val channels = new Channels(createChannel, closeChannel)

  private val bindings = new Bindings(
    handleLookup(Registry.Message.Request),
    handleLookup(Registry.Message.Response))

  val remoteJoined: Notification[RemoteRef] = connections.remoteJoined

  val remoteLeft: Notification[RemoteRef] = connections.remoteLeft

  def remotes: List[RemoteRef] = connections.remotes

  def running: Boolean = !connections.isTerminated

  def terminate(): Unit = connections.terminate


  private def createChannel(name: String, remote: RemoteRef) =
    Registry.Channel(name, remote, this)

  private def closeChannel(channel: Registry.Channel) =
    channel.doClosed()


  private def send(channel: Registry.Channel, message: MessageBuffer) =
    if (channel.open)
      connections send (
        channel.remote,
        Message(
          Registry.Message,
          Map(Registry.Message.Channel -> Seq(channel.name)),
          message))

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
        bindings processRequest (
          message, name, createAbstractionRef(name, channelName, remote))

      case (None, Some(Seq(name)), Some(Seq(channelName))) =>
        bindings processResponse (
          message, name, createAbstractionRef(name, channelName, remote))

      case (None, None, Some(Seq(channelName))) =>
        channels obtain (channelName, remote) doReceive message

      case _ =>
        // unknown message
    }
  }

  private def handleLookup(method: String)(
      abstraction: Registry.AbstractionRef, message: MessageBuffer) =
    connections send (
      abstraction.remote,
      Message(
        Registry.Message,
        Map(
          method -> Seq(abstraction.name),
          Registry.Message.Channel -> Seq(abstraction.channel.name)),
        message))

  private def createAbstractionRef(
      abstractionName: String, channelName: String, remote: RemoteRef) =
    Registry.AbstractionRef(
      abstractionName,
      remote,
      channels obtain (channelName, remote),
      this)


  def bindValue[T](name: String)(function: T)(
      implicit builder: BindingBuilder.Value[T]): Unit =
    bind(builder(name))(function)

  def bind[T](name: String)(function: T)(
      implicit builder: BindingBuilder[T]): Unit =
    bind(builder(name))(function)

  def bind[T](binding: Binding[T])(function: T): Unit =
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
      createAbstractionRef(
        binding.name,
        java.util.UUID.randomUUID.toString,
        remote))


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
