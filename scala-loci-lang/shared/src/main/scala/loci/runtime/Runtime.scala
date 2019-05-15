package loci
package runtime

import loci.communicator._
import loci.messaging._

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{CanAwait, ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

class Runtime[P](
    peer: Peer.Signature,
    ties: Runtime.Ties,
    context: ExecutionContext,
    connect: Runtime.Connections,
    system: Runtime.SystemFactory)
  extends loci.Runtime[P] {

  private val doStarted = Notifier[Instance[P]]

  private val doInstance = Promise[Instance[P]]

  private val doTerminated = Promise[Unit]

  private val remoteConnections = new RemoteConnections(peer, ties)

  private object state {
    private var running = false
    private var terminated = false

    def run() = running = true
    def terminate() = terminated = true
    def isRunning = running && !terminated
    def isTerminated = terminated

    val instances = mutable.ListBuffer.empty[Instance[P]]
  }

  private implicit val executionContext: ExecutionContext = contexts.Immediate.global

  @throws[IllegalArgumentException](
    "if the connection setup does not respect the tie specification")
  private def run(): Unit = state.synchronized {
    if (!state.isTerminated && !state.isRunning)
      try {
        if (ties.isEmpty && connect.nonEmpty)
          throw new IllegalArgumentException("no ties specified")

        val metaInstance = ties.toSeq.foldLeft(false) {
          case (metaInstance, (peer, multiplicity)) =>
            val subPeerConnects = connect collect {
              case (subPeer, (listeners, connectors)) if subPeer <= peer =>
                (listeners, connectors)
            }

            val (listeners, connectors) = subPeerConnects.unzip match {
              case (listeners, connectors) =>
                (listeners.flatten, connectors.flatten)
            }

            val connectCount = listeners.size + connectors.size

            if (multiplicity == Peer.Tie.Single && connectCount < 1)
              throw new IllegalArgumentException(
                s"no connection setup for single tie to ${peer.name}")

            if (multiplicity == Peer.Tie.Single && connectCount > 1)
              throw new IllegalArgumentException(
                s"more than one connection setup for single tie to ${peer.name}")

            if (multiplicity == Peer.Tie.Optional && connectCount > 1)
              throw new IllegalArgumentException(
                s"more than one connection setup for optional tie to ${peer.name}")

            metaInstance ||
            (listeners.nonEmpty &&
              (multiplicity == Peer.Tie.Single || multiplicity == Peer.Tie.Optional))
        }

        val listenersConnectors =
          connect.toSeq map { case (peer, (listeners, connectors)) =>
            (listeners map { _ -> peer }, connectors map { _ -> peer })
          }

        val (listeners, connectors) =
          listenersConnectors.unzip match { case (listeners, connectors) =>
            (listeners.flatten, connectors.flatten)
          }

        if (metaInstance) {
          if (listeners.size != 1)
            throw new IllegalArgumentException(
              "only one tie of single or optional type can be listened to")

          val (listener, peerType) = listeners.head

          remoteConnections.listenWithCallback(
              listener, peerType, createDesignatedInstance = true) { connection =>
            connection map { case (remote, remoteConnections) =>
              runPeer(remoteConnections, Seq(remote), Seq.empty, connectors)
            }
          }
        }
        else {
          remoteConnections.terminated notify { _ => terminate() }

          if (remoteConnections.isTerminated)
            terminate()
          else
            runPeer(remoteConnections, Seq.empty, listeners, connectors)
        }
      }
      catch {
        case NonFatal(e) =>
          state.terminate()
          doTerminated.success(())
          throw e
      }

      state.run()
  }

  private def runPeer(
      remoteConnections: RemoteConnections,
      requiredListenedRemotes: Seq[Remote.Reference],
      listeners: Seq[(Listener[ConnectionsBase.Protocol], Peer.Signature)],
      connectors: Seq[(Connector[messaging.ConnectionsBase.Protocol], Peer.Signature)]): Unit = {
    val requiredAndOptionalConnectors =
      connectors map { case connectorPeer @ (connector, peer) =>
        if (peer.bases exists { ties get _ contains Peer.Tie.Single })
          Left(connectorPeer)
        else
          Right(connectorPeer)
      }

    val requiredConnectors = requiredAndOptionalConnectors collect {
      case Left(connector) => connector
    }

    val optionalConnectors = requiredAndOptionalConnectors collect {
      case Right(connector) => connector
    }

    val future = Future.traverse(requiredConnectors) {
      case (connector, peer) =>
        remoteConnections connect (connector, peer)
    }

    future foreach { requiredConnectedRemotes =>
      val remotes = optionalConnectors map { case (connector, peer) =>
        remoteConnections connect (connector, peer)
      }

      listeners foreach { case (listener, peer) =>
        remoteConnections listen (listener, peer)
      }

      remoteConnections.terminated notify { _ =>
        state.synchronized {
          val index = state.instances indexWhere { _.remoteConnections eq remoteConnections }
          if (index != -1)
            state.instances.remove(index)
        }
      }

      context execute new Runnable {
        def run() = {
          val instance = state.synchronized {
            if (!state.isTerminated && remoteConnections.constraintViolations.isEmpty) {
              val values = system(
                ties, context, remoteConnections,
                requiredListenedRemotes ++ requiredConnectedRemotes, remotes)

              val instance = new Instance[P](values, remoteConnections)
              state.instances += instance
              Some(instance)
            }
            else
              None
          }

          instance foreach { instance =>
            doStarted(instance)
            doInstance.trySuccess(instance)
          }
        }
      }
    }

    future.failed foreach { exception =>
      context reportFailure exception
      remoteConnections.terminate()
    }
  }

  def terminate(): Unit = state.synchronized {
    if (!state.isTerminated) {
      state.terminate()
      state.instances foreach { _.terminate() }
      state.instances.clear

      remoteConnections.terminate()
      doTerminated.success(())
    }
  }

  val terminated: Future[Unit] = doTerminated.future

  val started: Notification[Instance[P]] = doStarted.notification

  val instance: Future[Instance[P]] = doInstance.future

  def instances: Seq[Instance[P]] = state.synchronized {
    state.instances.toList
  }

  override def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    terminated ready atMost
    this
  }

  override def result(atMost: Duration)(implicit permit: CanAwait): Unit =
    terminated result atMost
}

object Runtime {
  type Connections =
    Map[Peer.Signature, (
      List[Listener[ConnectionsBase.Protocol]],
      List[Connector[ConnectionsBase.Protocol]])]

  type Ties =
    Map[Peer.Signature, Peer.Tie]

  type SystemFactory =
    (Ties,
     ExecutionContext,
     RemoteConnections,
     Seq[Remote.Reference],
     Seq[Future[Remote.Reference]]) => PlacedValues

  @throws[IllegalArgumentException](
    "if the connection setup does not respect the tie specification")
  def start[P](
      peer: Peer.Signature,
      ties: Ties,
      context: ExecutionContext,
      connect: Connections,
      system: SystemFactory): Runtime[P] = {
    val runtime = new Runtime[P](peer, ties, context, connect, system)
    runtime.run()
    runtime
  }
}
