package retier
package impl

import AbstractionId._
import AbstractionRef._
import Channel._
import RemoteRef._
import Selection._
import transmission.MultipleTransmission
import transmission.OptionalTransmission
import transmission.SingleTransmission
import network.ConnectionRequestor
import util.Notifier
import util.Notification
import scala.ref.WeakReference
import scala.concurrent.Promise
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.collection.JavaConverters._
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference

class System(
    executionContext: ExecutionContext,
    remoteConnections: RemoteConnections,
    singleConnectedRemotes: Seq[RemoteRef],
    connectingRemotes: Seq[Future[RemoteRef]],
    peerImpl: PeerImpl.Ops) {

  private implicit val context = executionContext

  private val mainThread = new AtomicReference(Option.empty[Thread])

  private val pendingSingleConnectedRemotes =
    new ConcurrentHashMap[RemoteRef, Any]
  
  private var doneMain = false
  
  singleConnectedRemotes foreach { remote =>
    pendingSingleConnectedRemotes put (remote, remote)
  }

  private def doMain() = mainThread synchronized {
    if (!doneMain) {
      implicit val context = contexts.Queued.create
      doneMain = true

      Future {
        mainThread set Some(Thread.currentThread)
        try peerImpl.main
        catch {
          case _: InterruptedException if remoteConnections.isTerminated =>
        }
      }
    }
  }

  def main(): this.type = {
    dispatcher ignoreDispatched PendingConstruction

    if (singleConnectedRemotes.isEmpty)
      doMain()

    this
  }

  def running(): Boolean = !remoteConnections.isTerminated

  def terminate(): Unit = remoteConnections.terminate

  connectingRemotes foreach {
    _.failed foreach { _ => peerImpl.error }
  }




  // remote peer references

  private def isConnected(remote: RemoteRef): Boolean =
    remoteConnections isConnected remote

  private val doRemoteJoined = Notifier[RemoteRef]

  private val doRemotePreJoined = Notifier[RemoteRef]

  private val doRemoteLeft = Notifier[RemoteRef]

  private val doRemotePreLeft = Notifier[RemoteRef]

  private val singleRemotes = (singleConnectedRemotes flatMap { remote =>
    (bases(remote.peerType) + remote.peerType) map { _ -> remote }
  }).toMap

  private def bases(peerType: PeerType): Set[PeerType] =
    peerType.bases.toSet ++ (peerType.bases flatMap bases)


  def remotes(peerType: PeerType): List[RemoteRef] =
    (remoteConnections.remotes
      filter startedRemotes.containsKey
      map { _.asRemote(peerType) }).flatten

  def preRemotes(peerType: PeerType): List[RemoteRef] =
    (remoteConnections.remotes map { _.asRemote(peerType) }).flatten

  def remotes[R <: Peer: PeerTypeTag]: List[Remote[R]] =
    (remoteConnections.remotes
      filter startedRemotes.containsKey
      map { _.asRemote[R] }).flatten

  def preRemotes[R <: Peer: PeerTypeTag]: List[Remote[R]] =
    (remoteConnections.remotes map { _.asRemote[R] }).flatten

  def singleRemote(peerType: PeerType): RemoteRef =
    singleRemotes(peerType)

  def singlePreRemote(peerType: PeerType): RemoteRef =
    singleRemote(peerType)

  def singleRemote[R <: Peer: PeerTypeTag]: Remote[R] =
    singleRemotes(peerTypeOf[R]).asRemote[R].get

  def singlePreRemote[R <: Peer: PeerTypeTag]: Remote[R] =
    singleRemote[R]

  def optionalRemote(peerType: PeerType): Option[RemoteRef] =
    remotes(peerType).headOption

  def optionalPreRemote(peerType: PeerType): Option[RemoteRef] =
    preRemotes(peerType).headOption

  def optionalRemote[R <: Peer: PeerTypeTag]: Option[Remote[R]] =
    remotes(peerTypeOf[R]).headOption flatMap { _.asRemote[R] }

  def optionalPreRemote[R <: Peer: PeerTypeTag]: Option[Remote[R]] =
    preRemotes(peerTypeOf[R]).headOption flatMap { _.asRemote[R] }


  def remoteJoined(peerType: PeerType): Notification[RemoteRef] =
    doRemoteJoined.notification transform {
      Function unlift { _ asRemote peerType }
    }

  def remotePreJoined(peerType: PeerType): Notification[RemoteRef] =
    doRemotePreJoined.notification transform {
      Function unlift { _ asRemote peerType }
    }

  def remoteLeft(peerType: PeerType): Notification[RemoteRef] =
    doRemoteLeft.notification transform {
      Function unlift { _ asRemote peerType }
    }

  def remotePreLeft(peerType: PeerType): Notification[RemoteRef] =
    doRemotePreLeft.notification transform {
      Function unlift { _ asRemote peerType }
    }

  def remoteJoined[R <: Peer: PeerTypeTag]: Notification[Remote[R]] =
    doRemoteJoined.notification transform {
      Function unlift { _.asRemote[R] }
    }

  def remotePreJoined[R <: Peer: PeerTypeTag]: Notification[Remote[R]] =
    doRemotePreJoined.notification transform {
      Function unlift { _.asRemote[R] }
    }

  def remoteLeft[R <: Peer: PeerTypeTag]: Notification[Remote[R]] =
    doRemoteLeft.notification transform {
      Function unlift { _.asRemote[R] }
    }

  def remotePreLeft[R <: Peer: PeerTypeTag]: Notification[Remote[R]] =
    doRemotePreLeft.notification transform {
      Function unlift { _.asRemote[R] }
    }



  // connections

  def requestRemoteConnection
      [R <: Peer: PeerTypeTag](requestor: ConnectionRequestor): Unit =
    (remoteConnections request (requestor, peerTypeOf[R])).failed foreach {
      _ => peerImpl.error
    }

  def createMultipleRemoteConnection
      [R <: Peer: PeerTypeTag]: MultipleRemoteConnection[R] =
    MultipleRemoteConnectionImpl(this)

  def createOptionalRemoteConnection
      [R <: Peer: PeerTypeTag]: OptionalRemoteConnection[R] =
    OptionalRemoteConnectionImpl(this)

  def createSingleRemoteConnection
      [R <: Peer: PeerTypeTag]: SingleRemoteConnection[R] =
    SingleRemoteConnectionImpl(this)




  // memoization

  private val memos = new ConcurrentHashMap[Any, WeakReference[AnyRef]]

  private[impl] def memo[T <: AnyRef](id: Any, body: => T): T =
    Option(memos get id) collect {
      case WeakReference(value) => value.asInstanceOf[T]
    } getOrElse {
      val value = body
      memos put (id, WeakReference(value))
      value
    }




  // transmissions

  def executeTransmission[T, R <: Peer: PeerTypeTag]
      (props: TransmissionProperties[T]): Unit =
    requestRemotes(props, preRemotes[R], false)

  def executeTransmission[T, R <: Peer: PeerTypeTag]
      (selection: T fromMultiple R): Unit =
    requestRemotes(selection.props, preRemotes[R] filter selection.filter, false)

  def executeTransmission[T, R <: Peer: PeerTypeTag]
      (selection: T fromSingle R): Unit =
    requestRemotes(selection.props, preRemotes[R] filter selection.filter, false)


  def createMultipleTransmission
      [T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag]
      (props: TransmissionProperties[T]): MultipleTransmission[T, R, L] =
    MultipleTransmissionImpl(this, Selection(props, None))

  def createMultipleTransmission
      [T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag]
      (selection: T from R): MultipleTransmission[T, R, L] =
    MultipleTransmissionImpl(this, selection)

  def createMultipleTransmission
      [T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag]
      (selection: T fromMultiple R): MultipleTransmission[T, R, L] =
    MultipleTransmissionImpl(this, selection)

  def createOptionalTransmission
      [T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag]
      (props: TransmissionProperties[T]): OptionalTransmission[T, R, L] =
    OptionalTransmissionImpl(this, Selection(props, None))

  def createOptionalTransmission
      [T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag]
      (selection: T from R): OptionalTransmission[T, R, L] =
    OptionalTransmissionImpl(this, selection)

  def createSingleTransmission
      [T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag]
      (props: TransmissionProperties[T]): SingleTransmission[T, R, L] =
    SingleTransmissionImpl(this, Selection(props, None))

  def createSingleTransmission
      [T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag]
      (selection: T from R): SingleTransmission[T, R, L] =
    SingleTransmissionImpl(this, selection)

  def createSingleTransmission
      [T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag]
      (selection: T fromSingle R): SingleTransmission[T, R, L] =
    SingleTransmissionImpl(this, selection)




  // selections

  def createPeerSelection[T, P <: Peer: PeerTypeTag]
      (props: TransmissionProperties[T]): T from P =
    Selection(props, None)

  def createPeerSelection[T, P <: Peer: PeerTypeTag]
      (props: TransmissionProperties[T], peer: Remote[P]): T fromSingle P =
    Selection(props, Some(Seq(peer)))

  def createPeerSelection[T, P <: Peer: PeerTypeTag]
      (props: TransmissionProperties[T], peers: Remote[P]*): T fromMultiple P =
    Selection(props, Some(peers))




  // channels and remote access

  private val startedRemotes =
    new ConcurrentHashMap[RemoteRef, Any]
  private val channels =
    new ConcurrentHashMap[(String, RemoteRef), Channel]
  private val channelResponseHandlers =
    new ConcurrentHashMap[Channel, ConcurrentLinkedQueue[String => Unit]]
  private val pushedValues =
    new ConcurrentHashMap[(RemoteRef, AbstractionId), Future[_]]
  private val noResultRequested =
    Future failed new RetierImplementationError("no result value requested")

  private val dispatcher = new Dispatcher[SystemDispatch]

  dispatcher dispatch PendingConstruction


  def obtainChannel(name: String, remote: RemoteRef): Channel = {
    val channelId = (name, remote)
    val channel = Channel.create(name, remote, this)
    if (isConnected(remote)) {
      val obtainedChannel =
        Option(channels putIfAbsent (channelId, channel)) getOrElse channel

      if (!isConnected(remote))
        channels remove obtainedChannel

      obtainedChannel
    }
    else
      channel
  }

  def closeChannel(channel: Channel): Unit = {
    val channelId = (channel.name, channel.remote)
    Option(channels remove channelId) foreach { channel =>
      context execute new Runnable {
        def run = channel.closed()
      }
    }
  }

  def closeChannels(remote: RemoteRef): Unit = {
    channels.values.asScala.toSeq foreach { channel =>
      if (channel.remote == remote) {
        closeChannel(channel)
        channelResponseHandlers remove channel
      }
    }

    pushedValues.keys.asScala.toSeq foreach {
      case remoteAbstraction @ (`remote`, _) =>
        pushedValues remove remoteAbstraction
      case _ =>
    }
  }

  def isChannelOpen(channel: Channel): Boolean = {
    val channelId = (channel.name, channel.remote)
    channel == (channels get channelId)
  }

  def sendMessage(channel: Channel, messageType: String, payload: String): Unit =
    if (isChannelOpen(channel))
      remoteConnections send (
        channel.remote,
        ChannelMessage(messageType, channel.name, None, payload))


  remoteConnections.remotes foreach { remote =>
    startedRemotes put (remote, remote)
    dispatcher dispatch StartedMessageDispatch(remote)
  }

  remoteConnections.remoteJoined += { remote =>
    doRemotePreJoined(remote)
    dispatcher dispatch StartedMessageDispatch(remote)
  }

  remoteConnections.remoteLeft += { remote =>
    doRemotePreLeft(remote)
    context execute new Runnable {
      def run = {
        doRemoteLeft(remote)
        remote.doDisconnected
      }
    }
    closeChannels(remote)
  }

  remoteConnections.constraintsViolated += { _ =>
    context execute new Runnable {
      def run = peerImpl.fatal
    }
    remoteConnections.terminate
  }

  remoteConnections.terminated += { _ =>
    context execute new Runnable {
      def run = peerImpl.terminating
    }
    (mainThread getAndSet None) foreach { _.interrupt }
  }

  remoteConnections.receive += { case (remote, message) =>
    dispatcher dispatch MessageDispatch(remote, message)
  }


  sealed trait SystemDispatch extends Dispatch[SystemDispatch]

  case object PendingConstruction
    extends SystemDispatch with Undispatchable[SystemDispatch]

  case class StartedMessageDispatch(remote: RemoteRef)
      extends SystemDispatch {

    def blockedBy(dispatch: SystemDispatch) = false

    def run = remoteConnections send (remote, StartedMessage())
  }

  case class MessageDispatch(remote: RemoteRef, message: Message)
      extends SystemDispatch {

    def blockedBy(dispatch: SystemDispatch) = dispatch match {
      case MessageDispatch(otherRemote, _) => remote == otherRemote
      case StartedMessageDispatch(_) => false
      case PendingConstruction => true
    }

    def run = message match {
      case StartedMessage() =>
        if (Option(startedRemotes putIfAbsent (remote, remote)).isEmpty)
          context execute new Runnable {
            def run = doRemoteJoined(remote)
          }

          if (singleConnectedRemotes.nonEmpty) {
            pendingSingleConnectedRemotes remove remote
            if (pendingSingleConnectedRemotes.isEmpty)
              doMain()
          }

      case ChannelMessage(messageType @ ("Request" | "Call"), channelName, Some(abstraction), payload) =>
        val id = AbstractionId.create(abstraction)
        val ref = AbstractionRef.create(id, channelName, remote, System.this)
        context execute new Runnable {
          def run = peerImpl.dispatch(payload, id, ref) foreach { payload =>
            if (messageType == "Request")
              ref.channel send ("Response", payload)
          }
        }

      case ChannelMessage("Response", channelName, None, payload) =>
        val channel = obtainChannel(channelName, remote)
        Option(channelResponseHandlers get channel) foreach { handlers =>
          Option(handlers.poll) foreach { _(payload) }
        }

      case ChannelMessage(messageType, channelName, None, payload) =>
        val channel = obtainChannel(channelName, remote)
        channel receive (messageType, payload)

      case _ =>
    }
  }

  def createRemoteAbstractionRef(abstraction: AbstractionId,
      remote: RemoteRef): AbstractionRef =
    AbstractionRef.create(
      abstraction, java.util.UUID.randomUUID.toString, remote, this)

  def requestRemotes[T](props: TransmissionProperties[T],
      remotes: Seq[RemoteRef], requestResult: Boolean): Seq[Future[T]] = {
    def channelClosedException =
      new RemoteConnectionException("channel closed")
    def remoteNotConnectedException =
      new RemoteConnectionException("remote not connected")

    remotes map { remote =>
      val remoteAbstraction = (remote, props.abstraction)
      val abstraction = createRemoteAbstractionRef(props.abstraction, remote)
      val channel = abstraction.channel

      def sendRequest(messageType: String) =
        remoteConnections send (
          channel.remote,
          ChannelMessage(
            messageType,
            channel.name,
            Some(props.abstraction.name),
            props marshalRequest abstraction))

      if (!requestResult && (!props.isStable || !props.isPushBased)) {
        sendRequest("Call")
        noResultRequested
      }
      else {
        Option(pushedValues get remoteAbstraction) map {
          _.asInstanceOf[Future[T]]
        } getOrElse {
          val promise = Promise[T]
          val future = promise.future
          val pushValuesFuture =
            if (props.isStable && props.isPushBased)
              (Option(pushedValues putIfAbsent (remoteAbstraction, future))
                getOrElse future).asInstanceOf[Future[T]]
            else
              future

          if (pushValuesFuture eq future) {
            if (isConnected(remote)) {
              val handlers = Option(channelResponseHandlers get channel) getOrElse {
                val handlers = new ConcurrentLinkedQueue[String => Unit]
                Option(channelResponseHandlers putIfAbsent (channel, handlers)) getOrElse handlers
              }

              handlers add { response =>
                promise tryComplete (props unmarshalResponse (response, abstraction))
              }

              channel.closed += { _ =>
                promise tryFailure channelClosedException
              }

              if (!isChannelOpen(channel))
                promise tryFailure channelClosedException
              else
                sendRequest("Request")
            }
            else
              promise tryFailure remoteNotConnectedException

            future
          }
          else
            pushValuesFuture
        }
      }
    }
  }




  // start up system

  remoteConnections.run
}
