package retier
package impl

import RemoteRef._
import network.ConnectionListener
import network.ConnectionRequestor
import scala.util.Try
import scala.util.Success
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.ExecutionContext
import scala.concurrent.Awaitable
import scala.concurrent.CanAwait
import scala.concurrent.duration.Duration

class Runtime(
    connectionMultiplicities: Map[PeerType, ConnectionMultiplicity],
    peer: Peer,
    peerType: PeerType,
    peerSystem: Runtime.SystemFactory)
  extends retier.Runtime {

  private val remoteConnections =
    new RemoteConnections(peerType, connectionMultiplicities)

  private val peerConnectionSetup = peer.connect

  private val peerExecutionContext = peer.context

  private def bases(peerType: PeerType): Set[PeerType] =
    peerType.bases.toSet ++ (peerType.bases flatMap bases)

  private object state {
    private var running = false
    private var terminated = false

    def run() = running = true
    def terminate() = terminated = true
    def isRunning = running && !terminated
    def isTerminated = terminated

    val systems = ListBuffer.empty[System]
  }

  private implicit val executionContext = contexts.Immediate.global

  @throws[RemoteConnectionException](
    "if the connection setup does not respect the connection specification")
  private def run(): Unit = state.synchronized {
    if (!state.isTerminated && !state.isRunning)
      try {
        val peerTypes = connectionMultiplicities.keys flatMap { peerType =>
          bases(peerType) + peerType
        }

        val setup = peerConnectionSetup setup (peerType, peerTypes.toList)

        if (connectionMultiplicities.isEmpty && setup.nonEmpty)
          throw new RemoteConnectionException("no connections specified")

        val metaInstance = connectionMultiplicities.toSeq.foldLeft(false) {
          case (metaInstance, (peerType, multiplicity)) =>
            val peerSubTypeSetups = setup collect {
              case (peerSubType, (listeners, requestors))
                  if peerSubType <= peerType =>
                (listeners, requestors)
            }

            val (listeners, requestors) = peerSubTypeSetups.unzip match {
              case (listeners, requestors) =>
                (listeners.flatten, requestors.flatten)
            }

            val setupCount = listeners.size + requestors.size

            if (multiplicity == SingleConnection && setupCount < 1)
              throw new RemoteConnectionException("no connection " +
                s"setup for single connection to ${peerType.name}")

            if (multiplicity == SingleConnection && setupCount > 1)
              throw new RemoteConnectionException("more than one connection " +
                s"setup for single connection to ${peerType.name}")

            if (multiplicity == OptionalConnection && setupCount > 1)
              throw new RemoteConnectionException("more than one connection " +
                s"setup for optional connection to ${peerType.name}")

            metaInstance ||
            (listeners.nonEmpty &&
              (multiplicity == SingleConnection ||
               multiplicity == OptionalConnection))
        }

        val listenersRequestors =
          setup.toSeq map { case (peerType, (listeners, requestors)) =>
            (listeners map { _ -> peerType }, requestors map { _ -> peerType })
          }

        val (listeners, requestors) =
          listenersRequestors.unzip match { case (listeners, requestors) =>
            (listeners.flatten, requestors.flatten)
          }

        if (metaInstance) {
          if (listeners.size != 1)
            throw new RemoteConnectionException("only one connection " +
              "of single or optional type can be listened to")

          val (listener, peerType) = listeners.head
          val notification = remoteConnections listen
            (listener, peerType, createDesignatedInstance = true)

          notification += { connection =>
            connection map { case (remote, remoteConnections) =>
              runPeer(remoteConnections, Seq(remote), Seq.empty, requestors)
            }
          }
        }
        else {
          remoteConnections.terminated += { _ => terminate }

          if (remoteConnections.isTerminated)
            terminate
          else
            runPeer(remoteConnections, Seq.empty, listeners, requestors)
        }
      }
      catch {
        case exception: RemoteConnectionException =>
          state.terminate
          promise success (())
          throw exception
      }

      state.run
  }

  private def runPeer(remoteConnections: RemoteConnections,
      requiredListeners: Seq[RemoteRef],
      listeners: Seq[(ConnectionListener, PeerType)],
      requestors: Seq[(ConnectionRequestor, PeerType)]): Unit = {
    val requiredAndOptionalRequestors =
      requestors map { case requestorPeerType @ (requestor, peerType) =>
        val singleConnection = (bases(peerType) + peerType) exists { peerType =>
          (connectionMultiplicities get peerType) == Some(SingleConnection)
        }

        if (singleConnection)
          Left(requestorPeerType)
        else
          Right(requestorPeerType)
      }

    val requiredRequestors = requiredAndOptionalRequestors collect {
      case Left(requestor) => requestor
    }

    val optionalRequestors = requiredAndOptionalRequestors collect {
      case Right(requestor) => requestor
    }

    val future = Future.traverse(requiredRequestors) {
      case (requestor, peerType) =>
        remoteConnections request (requestor, peerType)
    }

    future onSuccess { case requiredRequestors =>
      optionalRequestors foreach { case (requestor, peerType) =>
        remoteConnections request (requestor, peerType) }

      listeners foreach { case (listener, peerType) =>
        remoteConnections listen (listener, peerType) }

      peerExecutionContext execute new Runnable {
        def run() = state.synchronized {
          if (!state.isTerminated &&
              remoteConnections.constraintViolations.isEmpty)
            state.systems += peerSystem(peerExecutionContext, remoteConnections,
              requiredListeners ++ requiredRequestors)
            remoteConnections.run
        }
      }
    }

    future onFailure { case exception =>
      peerExecutionContext reportFailure exception
    }
  }

  def terminate(): Unit = state.synchronized {
    if (!state.isTerminated) {
      state.terminate
      state.systems foreach { _.terminate }
      state.systems.clear

      remoteConnections.terminate
      promise success (())
    }
  }

  private val promise = Promise[Unit]

  val exited: Future[Unit] = promise.future

  override def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    exited ready atMost
    this
  }

  override def result(atMost: Duration)(implicit permit: CanAwait): Unit =
    exited result atMost
}

object Runtime {
  type SystemFactory =
    (ExecutionContext, RemoteConnections, Seq[RemoteRef]) => System

  @throws[RemoteConnectionException](
    "if the connection setup does not respect the connection specification")
  def run(
      connectionMultiplicities: Map[PeerType, ConnectionMultiplicity],
      peer: Peer,
      peerType: PeerType,
      peerSystem: SystemFactory): Runtime = {
    val runtime =
      new Runtime(connectionMultiplicities, peer, peerType, peerSystem)
    runtime.run
    runtime
  }
}
