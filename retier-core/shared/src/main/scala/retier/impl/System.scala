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
import util.Notification
import scala.annotation.tailrec
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

  def main(): this.type = {
    implicit val context = contexts.Queued.create

    Future {
      mainThread set Some(Thread.currentThread)
      try peerImpl.main
      catch {
        case _: InterruptedException if remoteConnections.isTerminated =>
      }
    }

    this
  }

  def running(): Boolean = !remoteConnections.isTerminated

  def terminate(): Unit = remoteConnections.terminate

  connectingRemotes foreach {
    _.failed foreach { _ => peerImpl.error }
  }




  // remote peer references

  val singleRemotes = (singleConnectedRemotes map { remote =>
    remote.peerType -> remote
  }).toMap

  def allRemotes: List[RemoteRef] = remoteConnections.remotes

  def remotes(peerType: PeerType): List[RemoteRef] =
    (remoteConnections.remotes map { _.asRemote(peerType) }).flatten

  def remotes[R <: Peer: PeerTypeTag]: List[Remote[R]] =
    (remoteConnections.remotes map { _.asRemote[R] }).flatten

  def singleRemote(peerType: PeerType): RemoteRef =
    singleRemotes(peerType)

  def singleRemote[R <: Peer: PeerTypeTag]: Remote[R] =
    singleRemotes(peerTypeOf[R]).asRemote[R].get

  def optionalRemote(peerType: PeerType): Option[RemoteRef] =
    remotes(peerType).headOption

  def optionalRemote[R <: Peer: PeerTypeTag]: Option[Remote[R]] =
    remotes(peerTypeOf[R]).headOption flatMap { _.asRemote[R] }

  def isConnected(remote: RemoteRef): Boolean =
    remoteConnections isConnected remote


  def anyRemoteJoined: Notification[RemoteRef] =
    remoteConnections.remoteJoined.inContext

  def anyRemoteLeft: Notification[RemoteRef] =
    remoteConnections.remoteLeft.inContext

  def remoteJoined(peerType: PeerType): Notification[RemoteRef] =
    anyRemoteJoined transformInContext {
      Function unlift { _ asRemote peerType }
    }

  def remoteLeft(peerType: PeerType): Notification[RemoteRef] =
    anyRemoteLeft transformInContext {
      Function unlift { _ asRemote peerType }
    }

  def remoteJoined[R <: Peer: PeerTypeTag]: Notification[Remote[R]] =
    anyRemoteJoined transformInContext {
      Function unlift { _.asRemote[R] }
    }

  def remoteLeft[R <: Peer: PeerTypeTag]: Notification[Remote[R]] =
    anyRemoteLeft transformInContext {
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
    requestRemotes(props, remotes[R])

  def executeTransmission[T, R <: Peer: PeerTypeTag]
      (selection: T fromMultiple R): Unit =
    requestRemotes(selection.props, remotes[R] filter selection.filter)

  def executeTransmission[T, R <: Peer: PeerTypeTag]
      (selection: T fromSingle R): Unit =
    requestRemotes(selection.props, remotes[R] filter selection.filter)


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

  def createOptionalTransmission
      [T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag]
      (selection: T fromSingle R): OptionalTransmission[T, R, L] =
    OptionalTransmissionImpl(this, selection)

  def createSingleTransmission
      [T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag]
      (props: TransmissionProperties[T]): SingleTransmission[T, R, L] =
    SingleTransmissionImpl(this, props)

  def createSingleTransmission
      [T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag]
      (selection: T from R): SingleTransmission[T, R, L] =
    SingleTransmissionImpl(this, selection.props)




  // selections

  def createPeerSelection[T, P <: Peer: PeerTypeTag]
      (props: TransmissionProperties[T]): T from P =
    Selection(props, None)

  def createPeerSelection[T, P <: Peer: PeerTypeTag]
      (props: TransmissionProperties[T], peer: Remote[P]): T fromSingle P =
    Selection(props, Some(Set(peer)))

  def createPeerSelection[T, P <: Peer: PeerTypeTag]
      (props: TransmissionProperties[T], peers: Remote[P]*): T fromMultiple P =
    Selection(props, Some(peers.toSet))




  // channels and remote access

  private val channels =
    new ConcurrentHashMap[(String, RemoteRef), Channel]
  private val channelResponseHandlers =
    new ConcurrentHashMap[Channel, String => Unit]
  private val pushedValues =
    new ConcurrentHashMap[(RemoteRef, AbstractionId), Future[_]]
  private val channelQueue =
    new ConcurrentLinkedQueue[(RemoteRef, Message)]

  def allChannels: List[Channel] = channels.values.asScala.toList

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

  def isChannelOpen(channel: Channel): Boolean =
    channels containsValue channel

  def sendMessage(channel: Channel, messageType: String, payload: String): Unit =
    if (isChannelOpen(channel))
      remoteConnections send (
        channel.remote,
        ContentMessage(messageType, channel.name, None, payload))


  remoteConnections.remoteLeft += { remote =>
    context execute new Runnable {
      def run = remote.doDisconnected
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

  remoteConnections.receive += { message =>
    message match {
      case (remote, ContentMessage(
          "Request", channelName, Some(abstraction), payload)) =>
        val id = AbstractionId.create(abstraction)
        val ref = AbstractionRef.create(id, channelName, remote, this)
        context execute new Runnable {
          def run = peerImpl.dispatch(payload, id, ref) foreach { payload =>
            ref.channel send ("Response", payload)
          }
        }

      case (remote, ContentMessage(
          "Response", channelName, None, payload)) =>
        val channel = obtainChannel(channelName, remote)
        Option(channelResponseHandlers remove channel) foreach { handle =>
          context execute new Runnable {
            def run = handle(payload)
          }
        }

      case message =>
        val executing = !channelQueue.isEmpty
        channelQueue add message

        if (!executing && !channelQueue.isEmpty)
          context execute new Runnable {
            def run = channelQueue synchronized {
              @tailrec def processQueue(): Unit =
                channelQueue.poll match {
                  case (remote, ContentMessage(
                      messageType, channelName, None, payload)) =>
                    val channel = obtainChannel(channelName, remote)
                    channel receive (messageType, payload)
                    processQueue

                  case message =>
                    if (Option(message).nonEmpty)
                      processQueue
                }

              processQueue
            }
          }
    }
  }


  def createRemoteAbstractionRef(abstraction: AbstractionId,
      remote: RemoteRef): AbstractionRef =
    AbstractionRef.create(
      abstraction, java.util.UUID.randomUUID.toString, remote, this)

  def requestRemotes[T](props: TransmissionProperties[T],
      remotes: Seq[RemoteRef]): Seq[Future[T]] = {
    remotes map { remote =>
      val remoteAbstraction = (remote, props.abstraction)
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
          val abstraction = createRemoteAbstractionRef(props.abstraction, remote)
          val channel = abstraction.channel

          channelResponseHandlers put (channel, { response =>
            promise tryComplete (props unmarshalResponse (response, abstraction))
          })

          channel.closed += { _ =>
            promise tryFailure new RemoteConnectionException("channel closed")
          }

          remoteConnections send (
            channel.remote,
            ContentMessage(
              "Request",
              channel.name,
              Some(props.abstraction.name),
              props marshalRequest abstraction))

          future
        }
        else
          pushValuesFuture
      }
    }
  }




  // start up system

  remoteConnections.run
}
