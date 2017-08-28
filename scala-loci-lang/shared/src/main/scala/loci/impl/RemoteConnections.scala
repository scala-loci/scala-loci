package loci
package impl

import messaging.ConnectionsBase
import messaging.Message
import RemoteRef._
import communicator.Connection
import communicator.Connector
import communicator.Listener
import communicator.Listening
import contexts.Immediate.Implicits.global
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

object RemoteConnections {
  type Protocol = ConnectionsBase.Protocol
}

class RemoteConnections(peerType: PeerType,
    tieMultiplicities: Map[PeerType, TieMultiplicity])
  extends ConnectionsBase[RemoteRef, Message[Method]] {

  protected def deserializeMessage(message: MessageBuffer) =
    Message deserialize message

  protected def serializeMessage(message: Message[Method]) =
    Message serialize message

  private def violatedException =
    new RemoteConnectionException("tie constraints violated")

  private def messageException(message: Message[Method]) =
    new Message.Exception(s"unexpected connect message: $message")

  private val multiplicities =
    (tieMultiplicities.keys flatMap bases map {
      _ -> MultipleTie }).toMap ++
    tieMultiplicities

  private def bases(peerType: PeerType): Set[PeerType] =
    peerType.bases.toSet ++ (peerType.bases flatMap bases)

  protected class State extends BaseState {
    private val counter = new AtomicLong(1)
    def createId = counter.getAndIncrement
    val potentials = new ListBuffer[PeerType]
  }

  protected val state = new State

  private val doConstraintsSatisfied = Notifier[Unit]

  private val doConstraintsViolated = Notifier[RemoteConnectionException]

  def constraintsSatisfied: Notification[Unit] =
    doConstraintsSatisfied.notification

  def constraintsViolated: Notification[RemoteConnectionException] =
    doConstraintsViolated.notification

  def connect(
      connector: Connector[RemoteConnections.Protocol],
      remotePeerType: PeerType): Future[RemoteRef] = {
    val promise = Promise[RemoteRef]
    connectWithCallback(connector, remotePeerType) { promise complete _ }
    promise.future
  }

  def connectWithCallback(
      connector: Connector[RemoteConnections.Protocol],
      remotePeerType: PeerType)(
      handler: Try[RemoteRef] => Unit): Unit = sync {
    if (!isTerminated) {
      if (constraintViolationsConnecting(remotePeerType).isEmpty) {
        state.potentials += remotePeerType

        connector.connect() {
          case Success(connection) =>
            val remote = RemoteRef.create(
              remotePeerType, state.createId, connection.protocol, this)

            var closedHandler: Notifiable[_] = null
            var receiveHandler: Notifiable[_] = null

            closedHandler = connection.closed notify { _ =>
              handler(Failure(terminatedException))
            }

            receiveHandler = connection.receive notify { data =>
              sync {
                state.potentials -= remotePeerType

                if (receiveHandler != null)
                  receiveHandler.remove
                if (closedHandler != null)
                  closedHandler.remove

                val handleAccept =
                  handleAcceptMessage(connection, remote)
                val handleRequest =
                  handleRequestMessage(connection, remotePeerType) andThen {
                    _ map { case (remote, _) => remote }
                  }

                val result = deserializeMessage(data) flatMap {
                  handleAccept orElse handleRequest orElse
                  handleUnknownMessage
                }

                if (result.isFailure)
                  connection.close

                afterSync { handler(result) }
              }
            }

            connection send serializeMessage(
              RequestMessage(
                PeerType serialize remotePeerType,
                PeerType serialize peerType))

          case Failure(exception) =>
            handler(Failure(exception))
        }
      }
      else
        handler(Failure(violatedException))
    }
    else
      handler(Failure(terminatedException))
  }

  def listen(
      listener: Listener[RemoteConnections.Protocol],
      remotePeerType: PeerType,
      createDesignatedInstance: Boolean = false): Unit =
    listenWithCallback(
      listener, remotePeerType, createDesignatedInstance) { _ => }

  def listenWithCallback(
      listener: Listener[RemoteConnections.Protocol],
      remotePeerType: PeerType,
      createDesignatedInstance: Boolean = false)(
      handler: Try[(RemoteRef, RemoteConnections)] => Unit): Try[Unit] =
  sync {
    if (!isTerminated) {
      val listening = listener.startListening() {
        case Success(connection) =>
          var receiveHandler: Notifiable[_] = null

          receiveHandler = connection.receive notify { data =>
            if (receiveHandler != null)
              receiveHandler.remove

            val handleRequest = handleRequestMessage(
              connection, remotePeerType, createDesignatedInstance)

            val result = deserializeMessage(data) flatMap {
              handleRequest orElse handleUnknownMessage
            }

            if (result.isFailure)
              connection.close

            handler(result)
          }

        case Failure(exception) =>
          handler(Failure(exception))
      }

      listening foreach addListening

      listening map { _ => ()}
    }
    else
      Failure(terminatedException)
  }

  private def handleRequestMessage(
      connection: Connection[RemoteConnections.Protocol],
      remotePeerType: PeerType,
      createDesignatedInstance: Boolean = false)
  : PartialFunction[Message[Method], Try[(RemoteRef, RemoteConnections)]] = {
    case RequestMessage(requested, requesting) =>
      sync {
        if (!isTerminated)
          PeerType deserialize requested flatMap { requestedPeerType =>
            PeerType deserialize requesting flatMap { requestingPeerType =>
              if (peerType <= requestedPeerType &&
                  requestingPeerType <= remotePeerType &&
                  constraintViolationsConnecting(remotePeerType).isEmpty) {
                val instance =
                  if (!createDesignatedInstance) this
                  else new RemoteConnections(peerType, tieMultiplicities)

                val remote = RemoteRef.create(remotePeerType,
                  instance.state.createId, connection.protocol, this)

                connection send serializeMessage(AcceptMessage())

                val result = instance.addConnection(remote, connection)

                result map { _ => (remote, instance) }
              }
              else
                Failure(violatedException)
            }
          }
        else
          Failure(terminatedException)
      }
  }

  private def handleAcceptMessage(
      connection: Connection[RemoteConnections.Protocol],
      remote: RemoteRef)
  : PartialFunction[Message[Method], Try[RemoteRef]] = {
    case AcceptMessage() =>
      sync {
        if (!isTerminated)
          addConnection(remote, connection) map { _ => remote }
        else
          Failure(terminatedException)
      }
  }

  private val handleUnknownMessage
    : PartialFunction[Message[Method], Try[Nothing]] = {
      case message => Failure(messageException(message))
    }

  override protected def addConnection(
      remote: RemoteRef,
      connection: Connection[ConnectionsBase.Protocol]) =
    sync {
      handleConstraintChanges {
        super.addConnection(remote, connection)
      }
    }

  override protected def removeConnection(remote: RemoteRef) =
    sync {
      handleConstraintChanges {
        super.removeConnection(remote)
      }
    }

  private def handleConstraintChanges[T](changeConnection: => T): T =
    if (!state.isTerminated) {
      val constraintsSatisfiedBefore = constraintViolations.isEmpty
      val result = changeConnection
      val constraintsSatisfiedAfter = constraintViolations.isEmpty

      if (!constraintsSatisfiedBefore && constraintsSatisfiedAfter)
        afterSync { doConstraintsSatisfied() }
      if (constraintsSatisfiedBefore && !constraintsSatisfiedAfter)
        afterSync { doConstraintsViolated(violatedException) }

      result
    }
    else
      changeConnection

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
    val remotePeerTypes = (remotes map { _.peerType }).toSeq
    val potentialPeerTypes =
      if (includePotentials) synchronized { state.potentials.toSeq }
      else Seq.empty

    (remotePeerTypes ++ potentialPeerTypes) flatMap { peerType =>
      bases(peerType) + peerType
    }
  }

  private def checkConstraints(peerType: PeerType, count: Int): Boolean =
    (bases(peerType) + peerType).toSeq collect (Function unlift { peerType =>
      multiplicities get peerType map {
        case MultipleTie => true
        case OptionalTie => count <= 1
        case SingleTie => count == 1
      }
    }) reduceOption { _ && _ } getOrElse false
}

