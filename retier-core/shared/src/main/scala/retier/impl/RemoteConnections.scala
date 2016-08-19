package retier
package impl

import RemoteRef._
import network.ProtocolInfo
import network.Connection
import network.ConnectionRequestor
import network.ConnectionListener
import util.Notifier
import util.Notification
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.ExecutionContext
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicBoolean

class RemoteConnections(peerType: PeerType,
    connectionMultiplicities: Map[PeerType, ConnectionMultiplicity]) {

  private def terminatedException =
    new RemoteConnectionException("remote connection terminated")

  private def violationException =
    new RemoteConnectionException("connection constraint violation")

  private def messageException(message: Message) =
    new MessageException(s"unexpected connect message: $message")

  private val multiplicities =
    (connectionMultiplicities.keys flatMap bases map {
      _ -> MultipleConnection }).toMap ++
    connectionMultiplicities

  private def bases(peerType: PeerType): Set[PeerType] =
    peerType.bases.toSet ++ (peerType.bases flatMap bases)

  private implicit val executionContext = contexts.Immediate.global

  private object state {
    private val running = new AtomicBoolean(false)
    private val terminated = new AtomicBoolean(false)
    private val counter = new AtomicLong(1)

    def run() = running set true
    def terminate() = terminated set true
    def isRunning = running.get && !terminated.get
    def isTerminated = terminated.get
    def createId = counter.getAndIncrement

    val messages = new ListBuffer[(RemoteRef, Message)]
    val listeners = new ListBuffer[ConnectionListener]
    val potentials = new ListBuffer[PeerType]
    val remotes = new ConcurrentLinkedQueue[RemoteRef]
    val connections = new ConcurrentHashMap[RemoteRef, Connection]

    val sync = new FairSync
  }

  private val doRemoteJoined = Notifier[RemoteRef]

  private val doRemoteLeft = Notifier[RemoteRef]

  private val doConstraintsSatisfied = Notifier[Unit]

  private val doConstraintsViolated = Notifier[RemoteConnectionException]

  private val doTerminated = Notifier[List[RemoteRef]]

  private val doReceive = Notifier[(RemoteRef, Message)]

  def remoteJoined: Notification[RemoteRef] = doRemoteJoined.notification

  def remoteLeft: Notification[RemoteRef] = doRemoteLeft.notification

  def constraintsSatisfied: Notification[Unit] =
    doConstraintsSatisfied.notification

  def constraintsViolated: Notification[RemoteConnectionException] =
    doConstraintsViolated.notification

  def terminated: Notification[List[RemoteRef]] = doTerminated.notification

  def remotes: List[RemoteRef] = state.remotes.asScala.toList

  def isConnected(remote: RemoteRef): Boolean =
    state.connections containsKey remote

  def request(connectionRequestor: ConnectionRequestor,
      remotePeerType: PeerType): Future[RemoteRef] =
    if (state.isTerminated)
      Future failed terminatedException
    else
      state sync {
        if (constraintViolationsConnecting(remotePeerType).isEmpty) {
          state.potentials += remotePeerType

          connectionRequestor.request flatMap { connection =>
            val remote = RemoteRef.create(
              remotePeerType, state.createId, connection.protocol, this)

            val promise = Promise[RemoteRef]
            val future = promise.future

            val receive = { data: String =>
              state sync {
                if (promise.isCompleted)
                  Message deserialize data map { doBufferedReceive(remote, _) }
                else {
                  val handleAccept =
                    handleAcceptMessage(connection, remote)
                  val handleRequest =
                    handleRequestMessage(connection, remotePeerType) andThen {
                      _ map { case (remote, _) => remote }
                    }

                  state.potentials -= remotePeerType

                  val result = Message deserialize data flatMap {
                    handleAccept orElse handleRequest orElse
                    handleUnknownMessage
                  }

                  state.potentials += remotePeerType

                  promise complete result
                }
              }
            }

            val closed = { _: Unit =>
              state sync {
                if (!promise.isCompleted)
                  promise failure terminatedException
              }
            }

            future onComplete { _ =>
              connection.receive -= receive
              connection.closed -= closed
              state sync { state.potentials -= remotePeerType }
            }

            connection.receive += receive
            connection.closed += closed

            connection send
              (Message serialize RequestMessage(
                PeerType serialize remotePeerType,
                PeerType serialize peerType))

            future
          }
        }
        else
          Future failed violationException
      }

  def listen(connectionListener: ConnectionListener, remotePeerType: PeerType,
      createDesignatedInstance: Boolean = false)
  : Notification[Try[(RemoteRef, RemoteConnections)]] = {
    val doNotify = Notifier[Try[(RemoteRef, RemoteConnections)]]

    connectionListener.connectionEstablished += { connection =>
      val doRemoveConnectionListener = Notifier[Unit]

      val receive = { data: String =>
        val handleRequest = handleRequestMessage(
          connection, remotePeerType, createDesignatedInstance)

        val result = Message deserialize data flatMap {
          handleRequest orElse handleUnknownMessage
        }

        doRemoveConnectionListener()

        if (result.isFailure)
          connection.close

        doNotify(result)
      }

      doRemoveConnectionListener.notification += { _ =>
        connection.receive -= receive
      }

      connection.receive += receive
    }

    state sync {
      if (!state.isTerminated) {
        state.listeners += connectionListener
        connectionListener.start
      }
    }

    doNotify.notification
  }

  private def handleRequestMessage(connection: Connection,
      remotePeerType: PeerType, createDesignatedInstance: Boolean = false)
  : PartialFunction[Message, Try[(RemoteRef, RemoteConnections)]] = {
    case RequestMessage(requested, requesting) =>
      state sync {
        if (!state.isTerminated)
          PeerType deserialize requested flatMap { requestedPeerType =>
            PeerType deserialize requesting flatMap { requestingPeerType =>
              if (peerType <= requestedPeerType &&
                  requestingPeerType <= remotePeerType &&
                  constraintViolationsConnecting(remotePeerType).isEmpty) {
                val instance =
                  if (!createDesignatedInstance) this
                  else new RemoteConnections(peerType, connectionMultiplicities)

                val remote = RemoteRef.create(remotePeerType,
                  instance.state.createId, connection.protocol, this)

                val result = addRemoteConnection(instance, remote, connection,
                  sendAcceptMessage = true)

                result map { _ => (remote, instance) }
              }
              else
                Failure(violationException)
            }
          }
        else
          Failure(terminatedException)
      }
  }

  private def handleAcceptMessage(connection: Connection, remote: RemoteRef)
  : PartialFunction[Message, Try[RemoteRef]] = {
    case AcceptMessage() =>
      state sync {
        if (!state.isTerminated) {
          val result = addRemoteConnection(this, remote, connection,
            sendAcceptMessage = false)
          result map { _ => remote }
        }
        else
          Failure(terminatedException)
      }
  }

  private val handleUnknownMessage: PartialFunction[Message, Try[Nothing]] = {
    case message => Failure(messageException(message))
  }

  private def addRemoteConnection(instance: RemoteConnections,
      remote: RemoteRef, connection: Connection,
      sendAcceptMessage: Boolean): Try[Unit] =
    handleConstraintChanges(instance) {
      instance.state.connections put (remote, connection)
      instance.state.remotes add remote

      if (sendAcceptMessage)
        connection send (Message serialize AcceptMessage())

      instance.doRemoteJoined(remote)

      connection.receive += {
        Message deserialize _ map { instance.doBufferedReceive(remote, _) }
      }

      connection.closed += { connection =>
        state sync {
          if (state.connections containsKey remote)
            removeRemoteConnection(instance, remote)
        }
      }

      if (!connection.isOpen) {
        removeRemoteConnection(instance, remote)
        Failure(terminatedException)
      }
      else
        Success(())
    }

  private def removeRemoteConnection(instance: RemoteConnections,
      remote: RemoteRef): Unit =
    handleConstraintChanges(instance) {
      instance.state.remotes remove remote
      instance.state.connections remove remote
      instance.doRemoteLeft(remote)
    }

  private def handleConstraintChanges[T]
      (instance: RemoteConnections)(changeConnection: => T): T = {
    val constraintsSatisfiedBefore = instance.constraintViolations.isEmpty

    val result = changeConnection

    val constraintsSatisfiedAfter = instance.constraintViolations.isEmpty

    if (!constraintsSatisfiedBefore && constraintsSatisfiedAfter)
      instance.doConstraintsSatisfied()
    if (constraintsSatisfiedBefore && !constraintsSatisfiedAfter)
      instance.doConstraintsViolated(violationException)

    result
  }

  def constraintViolationsConnecting(peerType: PeerType): Option[PeerType] = {
    val peerTypeCounts =
      connections(includePotentials = true) count { _ == peerType }

    if (!checkConstraints(peerType, 1 + peerTypeCounts))
      Some(peerType)
    else
      None
  }

  def constraintViolations: Set[PeerType] = {
    val peerTypeCounts =
      (multiplicities map { case (peerType, _) => (peerType, 0) }) ++
      (connections(includePotentials = false) groupBy identity map {
        case (peerType, list) => (peerType, list.size)
      })

    (peerTypeCounts collect { case (peerType, count)
      if !checkConstraints(peerType, count) => peerType
    }).toSet
  }

  private def connections(includePotentials: Boolean): Seq[PeerType] = {
    val remotePeerTypes = (state.remotes.asScala map { _.peerType }).toSeq
    val potentialPeerTypes =
      if (includePotentials) state sync { state.potentials.toSeq }
      else Seq.empty

    (remotePeerTypes ++ potentialPeerTypes) flatMap { peerType =>
      bases(peerType) + peerType
    }
  }

  private def checkConstraints(peerType: PeerType, count: Int): Boolean =
    (bases(peerType) + peerType).toSeq collect (Function unlift { peerType =>
      multiplicities get peerType map {
        case MultipleConnection => true
        case OptionalConnection => count <= 1
        case SingleConnection => count == 1
      }
    }) reduceOption { _ && _ } getOrElse false

  def run(): Unit =
    state sync {
      if (!state.isTerminated && !state.isRunning) {
        state.messages foreach { doReceive(_) }
        state.messages.clear
        state.run
      }
    }

  def terminate(): Unit =
    state sync {
      if (!state.isTerminated) {
        val lastRemotes = remotes

        state.terminate

        state.remotes.clear
        state.connections.asScala foreach { case (remote, connection) =>
          connection.close
        }
        state.connections.clear

        state.listeners foreach { _.stop }
        state.listeners.clear

        doTerminated(lastRemotes)
      }
    }

  def isRunning: Boolean = state.isRunning

  def isTerminated: Boolean = state.isTerminated

  def disconnect(remote: RemoteRef): Unit =
    if (!state.isTerminated)
      Option(state.connections get remote) foreach { _.close }

  def send(remote: RemoteRef, message: Message): Unit =
    if (!state.isTerminated)
      Option(state.connections get remote) foreach {
        _ send (Message serialize message)
      }

  def receive: Notification[(RemoteRef, Message)] = doReceive.notification

  private def doBufferedReceive(remote: RemoteRef, message: Message): Unit =
    if (!state.isTerminated) {
      if (state.isRunning)
        doReceive((remote, message))
      else
        state sync {
          if (state.isRunning)
            doReceive((remote, message))
          else
            state.messages += ((remote, message))
        }
    }
}
