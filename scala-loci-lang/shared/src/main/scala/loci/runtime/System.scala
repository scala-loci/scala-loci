package loci
package runtime

import communicator.Connector
import compatibility.jdkCollectionConverters._
import messaging.Message
import transmitter.{RemoteAccessException, RemoteRef}

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.ref.WeakReference
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

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
    connectingRemotes: Seq[Notice.Steady[Try[Remote.Reference]]]) {

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
            contexts.Queued.create()
          else
            contexts.Immediate.global

        doneMain = true

        Future {
          mainThread.set(Some(Thread.currentThread))
          try { main() }
          catch {
            case _: InterruptedException if remoteConnections.isTerminated =>
            case NonFatal(exception) => logging.reportException(exception)
          }
        }
      }
    }
  }

  def start(): Unit = {
    logging.info("multitier system started")

    dispatcher.ignoreDispatched(PendingConstruction)

    if (singleConnectedRemotes.isEmpty)
      doMain()
  }

  def running: Boolean = !remoteConnections.isTerminated

  def terminate(): Unit = remoteConnections.terminate()




  // remote peer references

  private def isConnected(remote: Remote.Reference): Boolean =
    remoteConnections.isConnected(remote)

  private val doRemoteJoined = Notice.Stream[Remote.Reference]

  private val doRemoteJoinedEarly = Notice.Stream[Remote.Reference]

  private val doRemoteLeft = Notice.Stream[Remote.Reference]

  private val doRemoteLeftEarly = Notice.Stream[Remote.Reference]

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
      earlyNotification: Notice.Stream[Remote.Reference],
      lateNotification: Notice.Stream[Remote.Reference],
      peer: Peer.Signature,
      remotes: Seq[RemoteRef],
      earlyAccess: Boolean): Notice.Stream[Remote.Reference] = {
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
      earlyAccess: Boolean): Notice.Stream[Remote.Reference] =
    remoteNotification(
      doRemoteJoinedEarly.notice, doRemoteJoined.notice,
      peer, remotes, earlyAccess)

  private[runtime] def remoteLeft(
      peer: Peer.Signature,
      remotes: Seq[RemoteRef],
      earlyAccess: Boolean): Notice.Stream[Remote.Reference] =
    remoteNotification(
      doRemoteLeftEarly.notice, doRemoteLeft.notice,
      peer, remotes, earlyAccess)




  // connections

  private[runtime] def connect(
      peer: Peer.Signature,
      connector: Connector[messaging.ConnectionsBase.Protocol]): Unit =
    remoteConnections.connect(connector, peer) foreach {
      case Success(_) =>
      case Failure(exception) =>
        logging.warn("could not connect to remote instance", exception)
    }




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
    new ConcurrentHashMap[Channel, (Boolean, Try[MessageBuffer] => Unit)]
  private val channelMessages =
    new ConcurrentHashMap[String, mutable.ListBuffer[Message[Method]]]
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

      context.execute(new Runnable {
        def run() = channel.doClosed.set()
      })
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
      bufferedSend(channel, ChannelMessage(ChannelMessage.Type.Update, channel.name, None, payload))

  private def bufferedSend(channel: Channel, message: Message[Method]): Unit = {
    val queued = Option(channelMessages get channel.anchor) exists { messages =>
      messages synchronized {
        val queued = channelMessages containsKey channel.anchor
        if (queued)
          messages += message
        queued
      }
    }

    if (!queued) {
      logging.trace(s"send update to ${channel.remote}: $message")
      remoteConnections.send(channel.remote, message)
    }
  }

  remoteConnections.remotes foreach { remote =>
    startedRemotes.put(remote, remote)
    dispatcher.dispatch(StartedMessageDispatch(remote))
  }

  remoteConnections.remoteJoined foreach { remote =>
    doRemoteJoinedEarly.fire(remote)
    dispatcher.dispatch(StartedMessageDispatch(remote))
  }

  remoteConnections.remoteLeft foreach { remote =>
    doRemoteLeftEarly.fire(remote)
    context.execute(new Runnable {
      def run() = {
        logging.trace(s"remote left: $remote")

        doRemoteLeft.fire(remote)
        remote.doDisconnected.set()
      }
    })
    closeChannels(remote)
  }

  remoteConnections.constraintsViolated foreach { _ =>
    logging.error("tie constraints violated")
    remoteConnections.terminate()
  }

  remoteConnections.terminated foreach { _ =>
    logging.info("multitier system terminated")
    mainThread.getAndSet(None) foreach { _.interrupt() }
  }

  remoteConnections.receive foreach { remoteMessage =>
    val (remote, message) = remoteMessage
    dispatcher.dispatch(MessageDispatch(remote, message))
  }


  sealed trait SystemDispatch extends Dispatch[SystemDispatch]

  case object PendingConstruction
    extends SystemDispatch with Undispatchable[SystemDispatch]

  case class StartedMessageDispatch(remote: Remote.Reference)
      extends SystemDispatch {

    def blockedBy(dispatch: SystemDispatch) = false

    def run() = remoteConnections.send(remote, StartedMessage())
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
        if (Option(startedRemotes.putIfAbsent(remote, remote)).isEmpty) {
          logging.trace(s"remote joined: $remote")

          context.execute(new Runnable {
            def run() = doRemoteJoined.fire(remote)
          })
        }

        if (singleConnectedRemotes.nonEmpty) {
          pendingSingleConnectedRemotes.remove(remote)
          if (pendingSingleConnectedRemotes.isEmpty)
            doMain()
        }

      case ChannelMessage(
          messageType @ (ChannelMessage.Type.Request | ChannelMessage.Type.Call),
          channelName,
          Some(abstraction),
          payload) =>
        val signature = Value.Signature.deserialize(abstraction)
        val reference = Value.Reference(channelName, channelName, remote, System.this)
        context.execute(new Runnable {
          def run() = {
            val messages = mutable.ListBuffer.empty[Message[Method]]
            channelMessages.put(channelName, messages)

            logging.trace(s"handling remote access for $signature from $remote over channel $channelName")

            val result = values.$loci$dispatch(payload, signature, reference)

            if (messageType == ChannelMessage.Type.Request) {
              val message = result match {
                case Success(payload) =>
                  ChannelMessage(ChannelMessage.Type.Response, channelName, None, payload)
                case Failure(exception) =>
                  logging.debug("propagating exception upon remote access to remote instance", exception)
                  val payload = MessageBuffer.encodeString(RemoteAccessException.serialize(exception))
                  ChannelMessage(ChannelMessage.Type.Failure, channelName, None, payload)
              }

              logging.trace(s"sending remote access response for $signature to $remote over channel $channelName")

              remoteConnections.send(remote, message)

              messages synchronized {
                messages foreach { message =>
                  logging.trace(s"send update to $remote: $message")
                  remoteConnections.send(remote, message)
                }
                messages.clear()
                channelMessages.remove(channelName)
              }
            }
            else
              result.failed foreach { exception =>
                logging.warn("local exception upon remote access", exception)
              }

            channelMessages.remove(channelName)
          }
        })

      case ChannelMessage(
          messageType @ (ChannelMessage.Type.Response | ChannelMessage.Type.Failure),
          channelName,
          None,
          payload) =>
        getChannel(channelName, remote) match {
          case None =>
            logging.warn(s"unprocessed message [channel not open]: $message")

          case Some(channel) =>
            channelResponseHandlers.remove(channel) match {
              case null =>
                logging.warn(s"unprocessed message [no handler]: $message")

              case (connected, handler) =>
                val buffer =
                  if (messageType == ChannelMessage.Type.Response) {
                    logging.trace(s"received response upon remote access from $remote over channel $channelName: $message")
                    Success(payload)
                  }
                  else {
                    val exception = RemoteAccessException.deserialize(payload.decodeString)
                    logging.debug("received exception upon remote access", exception)
                    Failure(exception)
                  }

                handler(buffer)

                if (!connected)
                  closeChannel(channel, notifyRemote = false)
            }
        }

      case ChannelMessage(ChannelMessage.Type.Update, channelName, None, payload) =>
        getChannel(channelName, remote) match {
          case None =>
            logging.warn(s"unprocessed message [channel not open]: $message")

          case Some(channel) =>
            logging.trace(s"received update from $remote: $message")
            channel.doReceive.fire(payload)
        }

      case CloseMessage(channelName) =>
        logging.trace(s"received update from $remote: $message")
        getChannel(channelName, remote) foreach { closeChannel(_, notifyRemote = false) }

      case _ =>
        logging.warn(s"unprocessed message: $message")
    }
  }

  def invokeRemoteAccess[U, T](
      arguments: U,
      placedValue: PlacedValue[U, _, _, T],
      peer: Peer.Signature,
      remotes: Seq[RemoteRef],
      requestResult: Boolean): Seq[T] = {

    def sendRequest(messageType: ChannelMessage.Type, reference: Value.Reference) = {
      logging.trace(
        s"sending remote access for ${placedValue.signature} to ${reference.remote} " +
        s"over channel ${reference.channelName}")

      remoteConnections.send(
        reference.remote,
        ChannelMessage(
          messageType,
          reference.channelName,
          Some(Value.Signature.serialize(placedValue.signature)),
          placedValue.arguments.marshal(arguments, reference)))
    }

    def createReference(remote: Remote.Reference) = {
      val id = java.util.UUID.randomUUID.toString
      Value.Reference(id, id, remote, this)
    }

    val references = remoteReferences(peer, remotes, earlyAccess = true)

    if (placedValue.stable && placedValue.result.connected)
      logging.trace(s"accessing remote value [cached] for ${placedValue.signature} on [${references mkString ", "}]")
    else
      logging.trace(s"accessing remote value [uncached] for ${placedValue.signature} on [${references mkString ", "}]")

    if (!requestResult && (!placedValue.stable || !placedValue.result.connected)) {
      references foreach { remote =>
        sendRequest(ChannelMessage.Type.Call, createReference(remote))
      }
      Seq.empty
    }
    else
      references map { remote =>
        val remoteSignature = remote -> placedValue.signature
        val reference = createReference(remote)
        val channel = reference.channel

        val value = new System.ValueCell[T] {
          lazy val value = {
            val response = Notice.Steady[Try[MessageBuffer]]

            if (isConnected(remote)) {
              channelResponseHandlers.put(
                channel,
                placedValue.result.connected -> { response.trySet(_) })

              channel.closed foreach { _ =>
                response.trySet(Failure(new RemoteAccessException(RemoteAccessException.ChannelClosed)))
              }

              if (!isChannelOpen(channel))
                response.trySet(Failure(new RemoteAccessException(RemoteAccessException.ChannelClosed)))
              else
                sendRequest(ChannelMessage.Type.Request, reference)
            }
            else
              response.trySet(Failure(new RemoteAccessException(RemoteAccessException.RemoteDisconnected)))

            placedValue.result.unmarshal(response.notice, reference)
          }
        }

        if (placedValue.stable && placedValue.result.connected) {
          val cached = Option[System.ValueCell[_]](pushedValues.putIfAbsent(remoteSignature, value))

          if (cached.nonEmpty)
            logging.trace(s"using cached remote value for ${placedValue.signature} on $remote")

          (cached getOrElse value match {
            case value: System.ValueCell[T] @unchecked =>
              value.value
          }): T
        }
        else
          value.value
      }
  }




  // start up system

  remoteConnections.run()
}
