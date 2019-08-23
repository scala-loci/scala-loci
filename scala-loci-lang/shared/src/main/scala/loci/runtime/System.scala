package loci
package runtime

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import loci.communicator.Connector
import loci.messaging.Message
import loci.transmitter.{RemoteAccessException, RemoteRef}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.ref.WeakReference
import scala.util.Success

object System {
  private trait ValueCell[T] {
    def value: T
  }
}

class System(
    values: PlacedValues,
    main: Option[() => Unit],
    separateMainThread: Boolean,
    ties: Map[Peer.Signature, Peer.Tie],
    executionContext: ExecutionContext,
    remoteConnections: RemoteConnections,
    singleConnectedRemotes: Seq[Remote.Reference],
    connectingRemotes: Seq[Future[Remote.Reference]]) {

  private implicit val context: ExecutionContext = executionContext

  private val mainThread = new AtomicReference(Option.empty[Thread])

  private val pendingSingleConnectedRemotes =
    new ConcurrentHashMap[Remote.Reference, Any]

  private var doneMain = false

  singleConnectedRemotes foreach { remote =>
    pendingSingleConnectedRemotes.put(remote, remote)
  }

  private def doMain(): Unit = main foreach { main =>
    mainThread synchronized {
      if (!doneMain) {
        implicit val context: ExecutionContext =
          if (separateMainThread)
            contexts.Queued.create
          else
            contexts.Immediate.global

        doneMain = true

        Future {
          mainThread set Some(Thread.currentThread)
          try { main() }
          catch {
            case _: InterruptedException if remoteConnections.isTerminated =>
          }
        }
      }
    }
  }

  def start(): Unit = {
    dispatcher ignoreDispatched PendingConstruction

    if (singleConnectedRemotes.isEmpty)
      doMain()
  }

  def running(): Boolean = !remoteConnections.isTerminated

  def terminate(): Unit = remoteConnections.terminate()




  // remote peer references

  private def isConnected(remote: Remote.Reference): Boolean =
    remoteConnections isConnected remote

  private val doRemoteJoined = Notifier[Remote.Reference]

  private val doRemoteJoinedEarly = Notifier[Remote.Reference]

  private val doRemoteLeft = Notifier[Remote.Reference]

  private val doRemoteLeftEarly = Notifier[Remote.Reference]

  private val singleRemotes = (singleConnectedRemotes flatMap { remote =>
    remote.signature.bases map { _ -> remote }
  }).toMap

  private[runtime] def remoteReferences(
      peer: Peer.Signature,
      remotes: Seq[RemoteRef],
      earlyAccess: Boolean): Seq[Remote.Reference] =
    if (remotes.isEmpty) {
      def remotes =
        remoteConnections.remotes filter { remote =>
          (earlyAccess || (startedRemotes containsKey remote)) && (remote.signature <= peer)
        }

      (Peer.Tie(peer, ties) map {
        case Peer.Tie.Multiple => remotes
        case Peer.Tie.Optional => remotes.headOption.toList
        case Peer.Tie.Single => List(singleRemotes(peer))
      }
      getOrElse List.empty)
    }
    else
      remotes map {
        case remote: Remote.Reference => remote
        case _ => throw new PeerImplementationError
      }

  private def remoteNotification(
      earlyNotification: Notification[Remote.Reference],
      lateNotification: Notification[Remote.Reference],
      peer: Peer.Signature,
      remotes: Seq[RemoteRef],
      earlyAccess: Boolean): Notification[Remote.Reference] = {
    val notification =
      if (earlyAccess)
        earlyNotification
      else
        lateNotification

    if (remotes.isEmpty)
      notification filter { _.signature <= peer }
    else
      notification filter { remote =>
        remote.signature <= peer && (remotes contains remote)
      }
  }

  private[runtime] def remoteJoined(
      peer: Peer.Signature,
      remotes: Seq[RemoteRef],
      earlyAccess: Boolean): Notification[Remote.Reference] =
    remoteNotification(
      doRemoteJoinedEarly.notification, doRemoteJoined.notification,
      peer, remotes, earlyAccess)

  private[runtime] def remoteLeft(
      peer: Peer.Signature,
      remotes: Seq[RemoteRef],
      earlyAccess: Boolean): Notification[Remote.Reference] =
    remoteNotification(
      doRemoteLeftEarly.notification, doRemoteLeft.notification,
      peer, remotes, earlyAccess)




  // connections

  private[runtime] def connect(
      peer: Peer.Signature,
      connector: Connector[messaging.ConnectionsBase.Protocol]): Unit =
    remoteConnections.connect(connector, peer)




  // value caching

  private val cache = new ConcurrentHashMap[Any, WeakReference[AnyRef]]

  private[runtime] def cache[T <: AnyRef](id: Any, body: => T): T =
    Option(cache get id) collect {
      case WeakReference(value: T @unchecked) => value
    } getOrElse {
      val value = body
      cache.put(id, WeakReference(value))
      value
    }




  // subjective values

  def subjectiveValue[T, P](function: Remote[P] => T, remote: Remote[P]): T = {
    val valueId = remote -> function
    val value = cache(valueId, new System.ValueCell[T] { lazy val value = function(remote) })

    def connected = remote match {
      case remote: Remote.Reference => isConnected(remote)
      case _ => true
    }

    if (connected) {
      val result: T =
        Option(subjectiveValues.putIfAbsent(valueId, value)) getOrElse value match {
          case value: System.ValueCell[T] @unchecked =>
            value.value
        }

      if (!connected)
        subjectiveValues.remove(valueId)

      result
    }
    else
      value.value
  }




  // channels and remote access

  private val startedRemotes =
    new ConcurrentHashMap[Remote.Reference, Any]
  private val channels =
    new ConcurrentHashMap[(String, RemoteRef), Channel]
  private val channelResponseHandlers =
    new ConcurrentHashMap[Channel, (Boolean, MessageBuffer => Unit)]
  private val channelMessages =
    new ConcurrentHashMap[String, ListBuffer[Message[Method]]]
  private val pushedValues =
    new ConcurrentHashMap[(Remote[Any], Value.Signature), System.ValueCell[_]]
  private val subjectiveValues =
    new ConcurrentHashMap[(Remote[Any], Remote[Nothing] => Any), System.ValueCell[_]]

  private val dispatcher = new Dispatcher[SystemDispatch]

  dispatcher.dispatch(PendingConstruction)


  private[runtime] def obtainChannel(
      name: String,
      anchorDefault: String,
      remote: Remote.Reference): Channel = {
    val channelId = name -> remote
    val channel = Channel(name, anchorDefault, remote, this)
    if (isConnected(remote)) {
      val obtainedChannel =
        Option(channels.putIfAbsent(channelId, channel)) getOrElse channel

      if (!isConnected(remote))
        channels.remove(obtainedChannel)

      obtainedChannel
    }
    else
      channel
  }

  private[runtime] def getChannel(
      name: String,
      remote: Remote.Reference): Option[Channel] = {
    val channelId = name -> remote
    if (remote.connected)
      Option(channels get channelId)
    else
      None
  }

  private[runtime] def closeChannel(channel: Channel, notifyRemote: Boolean): Unit = {
    val channelId = (channel.name, channel.remote)

    Option(channels.remove(channelId)) foreach { channel =>
      if (notifyRemote)
        bufferedSend(channel, CloseMessage(channel.name))

      context execute new Runnable {
        def run() = channel.doClosed()
      }
    }
  }

  private[runtime] def closeChannels(remote: Remote.Reference): Unit = {
    channels.values.asScala.toSeq foreach { channel =>
      if (channel.remote == remote) {
        closeChannel(channel, notifyRemote = false)
        channelResponseHandlers.remove(channel)
      }
    }

    pushedValues.keys.asScala.toSeq foreach {
      case value @ (`remote`, _) =>
        pushedValues.remove(value)
      case _ =>
    }

    subjectiveValues.keys.asScala.toSeq foreach {
      case value @ (`remote`, _) =>
        subjectiveValues.remove(value)
      case _ =>
    }
  }

  private[runtime] def isChannelOpen(channel: Channel): Boolean = {
    val channelId = (channel.name, channel.remote)
    channel == (channels get channelId)
  }

  private[runtime] def sendMessage(channel: Channel, payload: MessageBuffer): Unit =
    if (isChannelOpen(channel))
      bufferedSend(channel, ChannelMessage("Update", channel.name, None, payload))

  def bufferedSend(channel: Channel, message: Message[Method]): Unit = {
    val queued = Option(channelMessages get channel.anchor) exists { messages =>
      messages synchronized {
        val queued = channelMessages containsKey channel.anchor
        if (queued)
          messages += message
        queued
      }
    }

    if (!queued)
      remoteConnections send (channel.remote, message)
  }

  remoteConnections.remotes foreach { remote =>
    startedRemotes.put(remote, remote)
    dispatcher.dispatch(StartedMessageDispatch(remote))
  }

  remoteConnections.remoteJoined notify { remote =>
    doRemoteJoinedEarly(remote)
    dispatcher.dispatch(StartedMessageDispatch(remote))
  }

  remoteConnections.remoteLeft notify { remote =>
    doRemoteLeftEarly(remote)
    context execute new Runnable {
      def run() = {
        doRemoteLeft(remote)
        remote.doDisconnected()
      }
    }
    closeChannels(remote)
  }

  remoteConnections.constraintsViolated notify { _ =>
    remoteConnections.terminate()
  }

  remoteConnections.terminated notify { _ =>
    (mainThread getAndSet None) foreach { _.interrupt }
  }

  remoteConnections.receive notify { remoteMessage =>
    val (remote, message) = remoteMessage
    dispatcher.dispatch(MessageDispatch(remote, message))
  }


  sealed trait SystemDispatch extends Dispatch[SystemDispatch]

  case object PendingConstruction
    extends SystemDispatch with Undispatchable[SystemDispatch]

  case class StartedMessageDispatch(remote: Remote.Reference)
      extends SystemDispatch {

    def blockedBy(dispatch: SystemDispatch) = false

    def run() = remoteConnections send (remote, StartedMessage())
  }

  case class MessageDispatch(remote: Remote.Reference, message: Message[Method])
      extends SystemDispatch {

    def blockedBy(dispatch: SystemDispatch) = dispatch match {
      case MessageDispatch(otherRemote, _) => remote == otherRemote
      case StartedMessageDispatch(_) => false
      case PendingConstruction => true
    }

    def run() = message match {
      case StartedMessage() =>
        if (Option(startedRemotes.putIfAbsent(remote, remote)).isEmpty)
          context execute new Runnable {
            def run() = doRemoteJoined(remote)
          }

          if (singleConnectedRemotes.nonEmpty) {
            pendingSingleConnectedRemotes.remove(remote)
            if (pendingSingleConnectedRemotes.isEmpty)
              doMain()
          }

      case ChannelMessage(
          messageType @ ("Request" | "Call"),
          channelName,
          Some(abstraction),
          payload) =>
        val signature = Value.Signature.deserialize(abstraction)
        val reference = Value.Reference(channelName, channelName, remote, System.this)
        context execute new Runnable {
          def run() = {
            val messages = ListBuffer.empty[Message[Method]]
            channelMessages.put(channelName, messages)

            values.$loci$dispatch(payload, signature, reference) foreach { payload =>
              if (messageType == "Request") {
                remoteConnections send (
                  remote,
                  ChannelMessage("Response", channelName, None, payload))

                messages synchronized {
                  messages foreach { remoteConnections send (reference.remote, _) }
                  messages.clear()
                  channelMessages.remove(channelName)
                }
              }
            }

            channelMessages.remove(channelName)
          }
        }

      case ChannelMessage("Response", channelName, None, payload) =>
        getChannel(channelName, remote) foreach { channel =>
          Option(channelResponseHandlers remove channel) foreach { case (connected, handler) =>
            handler(payload)
            if (!connected)
              closeChannel(channel, notifyRemote = false)
          }
        }

      case ChannelMessage("Update", channelName, None, payload) =>
        getChannel(channelName, remote) foreach { _.doReceive(payload) }

      case CloseMessage(channelName) =>
        getChannel(channelName, remote) foreach { closeChannel(_, notifyRemote = false) }

      case _ =>
    }
  }

  def invokeRemoteAccess[U, T](
      arguments: U,
      placedValue: PlacedValue[U, T],
      peer: Peer.Signature,
      remotes: Seq[RemoteRef],
      requestResult: Boolean): Seq[T] = {

    def sendRequest(messageType: String, reference: Value.Reference) =
      remoteConnections send (
        reference.remote,
        ChannelMessage(
          messageType,
          reference.channelName,
          Some(Value.Signature.serialize(placedValue.signature)),
          placedValue.arguments.marshal(arguments, reference)))

    def createReference(remote: Remote.Reference) = {
      val id = java.util.UUID.randomUUID.toString
      Value.Reference(id, id, remote, this)
    }

    if (!requestResult && (!placedValue.stable || !placedValue.result.connected)) {
      remoteReferences(peer, remotes, earlyAccess = true) foreach { remote =>
        sendRequest("Call", createReference(remote))
      }
      Seq.empty
    }
    else
      remoteReferences(peer, remotes, earlyAccess = true) map { remote =>
        val remoteSignature = remote -> placedValue.signature
        val reference = createReference(remote)
        val channel = reference.channel

        val value = new System.ValueCell[T] {
          lazy val value = {
            val promise = Promise[MessageBuffer]

            if (isConnected(remote)) {
              channelResponseHandlers.put(
                channel,
                placedValue.result.connected -> { promise tryComplete Success(_) })

              channel.closed notify { _ =>
                promise tryFailure new RemoteAccessException(RemoteAccessException.ChannelClosed)
              }

              if (!isChannelOpen(channel))
                promise tryFailure new RemoteAccessException(RemoteAccessException.ChannelClosed)
              else
                sendRequest("Request", reference)
            }
            else
              promise tryFailure new RemoteAccessException(RemoteAccessException.RemoteDisconnected)

            placedValue.result.unmarshal(promise.future, reference)
          }
        }

        if (placedValue.stable && placedValue.result.connected)
          (Option(pushedValues.putIfAbsent(remoteSignature, value)) getOrElse value match {
            case value: System.ValueCell[T] @unchecked =>
              value.value
          }): T
        else
          value.value
      }
  }




  // start up system

  remoteConnections.run()
}
