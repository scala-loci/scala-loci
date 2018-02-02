package loci
package impl

import RemoteRef._
import communicator.Listener
import communicator.Connector
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.ExecutionContext
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
              case (peerSubType, (listeners, connectors))
                  if peerSubType <= peerType =>
                (listeners, connectors)
            }

            val (listeners, connectors) = peerSubTypeSetups.unzip match {
              case (listeners, connectors) =>
                (listeners.flatten, connectors.flatten)
            }

            val setupCount = listeners.size + connectors.size

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

        val listenersConnectors =
          setup.toSeq map { case (peerType, (listeners, connectors)) =>
            (listeners map { _ -> peerType }, connectors map { _ -> peerType })
          }

        val (listeners, connectors) =
          listenersConnectors.unzip match { case (listeners, connectors) =>
            (listeners.flatten, connectors.flatten)
          }

        if (metaInstance) {
          if (listeners.size != 1)
            throw new RemoteConnectionException("only one tie " +
              "of single or optional type can be listened to")

          val (listener, peerType) = listeners.head

          remoteConnections.listenWithCallback(listener, peerType, createDesignatedInstance = true) { connection =>
            connection map { case (remote, remoteConnections) =>
              runPeer(remoteConnections, Seq(remote), Seq.empty, connectors)
            }
          }
        }
        else {
          remoteConnections.terminated notify { _ => terminate }

          if (remoteConnections.isTerminated)
            terminate
          else
            runPeer(remoteConnections, Seq.empty, listeners, connectors)
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
      listeners: Seq[(Listener[RemoteConnections.Protocol], PeerType)],
      connectors: Seq[(Connector[RemoteConnections.Protocol], PeerType)]): Unit = {
    val requiredAndOptionalConnectors =
      connectors map { case connectorPeerType @ (connector, peerType) =>
        val singleTie = (bases(peerType) + peerType) exists { peerType =>
          (tieMultiplicities get peerType) == Some(SingleTie)
        }

        if (singleTie)
          Left(connectorPeerType)
        else
          Right(connectorPeerType)
      }

    val requiredConnectors = requiredAndOptionalConnectors collect {
      case Left(connector) => connector
    }

    val optionalConnectors = requiredAndOptionalConnectors collect {
      case Right(connector) => connector
    }

    val future = Future.traverse(requiredConnectors) {
      case (connector, peerType) =>
        remoteConnections connect (connector, peerType)
    }

    future foreach { requiredConnectedRemotes =>
      val remotes = optionalConnectors map { case (connector, peerType) =>
        remoteConnections connect (connector, peerType) }

      listeners foreach { case (listener, peerType) =>
        remoteConnections listen (listener, peerType) }

      peerExecutionContext execute new Runnable {
        def run() = state.synchronized {
          if (!state.isTerminated &&
              remoteConnections.constraintViolations.isEmpty)
            state.systems += peerSystem(peerExecutionContext, remoteConnections,
              requiredListenedRemotes ++ requiredConnectedRemotes, remotes)
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
