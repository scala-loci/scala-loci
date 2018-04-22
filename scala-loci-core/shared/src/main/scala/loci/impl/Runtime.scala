package loci
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
    tieMultiplicities: Map[PeerType, TieMultiplicity],
    peer: Peer,
    peerType: PeerType,
    peerSystem: Runtime.SystemFactory)
  extends loci.Runtime {

  private val remoteConnections =
    new RemoteConnections(peerType, tieMultiplicities)

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
    "if the connection setup does not respect the tie specification")
  private def run(): Unit = state.synchronized {
    if (!state.isTerminated && !state.isRunning)
      try {
        val peerTypes = tieMultiplicities.keys flatMap { peerType =>
          bases(peerType) + peerType
        }

        val setup = peerConnectionSetup setup (peerType, peerTypes.toList)

        if (tieMultiplicities.isEmpty && setup.nonEmpty)
          throw new RemoteConnectionException("no ties specified")

        val metaInstance = tieMultiplicities.toSeq.foldLeft(false) {
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

            if (multiplicity == SingleTie && setupCount < 1)
              throw new RemoteConnectionException("no connection " +
                s"setup for single tie to ${peerType.name}")

            if (multiplicity == SingleTie && setupCount > 1)
              throw new RemoteConnectionException("more than one connection " +
                s"setup for single tie to ${peerType.name}")

            if (multiplicity == OptionalTie && setupCount > 1)
              throw new RemoteConnectionException("more than one connection " +
                s"setup for optional tie to ${peerType.name}")

            metaInstance ||
            (listeners.nonEmpty &&
              (multiplicity == SingleTie || multiplicity == OptionalTie))
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
            throw new RemoteConnectionException("only one tie " +
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
      requiredListenedRemotes: Seq[RemoteRef],
      listeners: Seq[(ConnectionListener, PeerType)],
      requestors: Seq[(ConnectionRequestor, PeerType)]): Unit = {
    val requiredAndOptionalRequestors =
      requestors map { case requestorPeerType @ (requestor, peerType) =>
        val singleTie = (bases(peerType) + peerType) exists { peerType =>
          (tieMultiplicities get peerType) == Some(SingleTie)
        }

        if (singleTie)
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

    future foreach { requiredRequestedRemotes =>
      val remotes = optionalRequestors map { case (requestor, peerType) =>
        remoteConnections request (requestor, peerType) }

      listeners foreach { case (listener, peerType) =>
        remoteConnections listen (listener, peerType) }

      peerExecutionContext execute new Runnable {
        def run() = state.synchronized {
          if (!state.isTerminated &&
              remoteConnections.constraintViolations.isEmpty)
            state.systems += peerSystem(peerExecutionContext, remoteConnections,
              requiredListenedRemotes ++ requiredRequestedRemotes, remotes)
        }
      }
    }

    future.failed foreach { exception =>
      peerExecutionContext reportFailure exception
      remoteConnections.terminate
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

  val terminated: Future[Unit] = promise.future

  override def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    terminated ready atMost
    this
  }

  override def result(atMost: Duration)(implicit permit: CanAwait): Unit =
    terminated result atMost
}

object Runtime {
  type SystemFactory =
    (ExecutionContext, RemoteConnections,
     Seq[RemoteRef], Seq[Future[RemoteRef]]) => System

  @throws[RemoteConnectionException](
    "if the connection setup does not respect the tie specification")
  def run(
      tieMultiplicities: Map[PeerType, TieMultiplicity],
      peer: Peer,
      peerType: PeerType,
      peerSystem: SystemFactory): Runtime = {
    val runtime =
      new Runtime(tieMultiplicities, peer, peerType, peerSystem)
    runtime.run
    runtime
  }
}
