package loci
package runtime

import communicator._
import messaging._

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{CanAwait, ExecutionContext, TimeoutException}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

class Runtime[P](
    peer: Peer.Signature,
    ties: Runtime.Ties,
    context: ExecutionContext,
    connect: Runtime.Connections,
    system: Runtime.SystemFactory)
  extends loci.Runtime[P] {

  private val doStarted = Notice.Stream[Instance[P]]

  private val doInstance = Notice.Steady[Instance[P]]

  private val doTerminated = Notice.Steady[Unit]

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

          logging.info(s"multitier runtime started for peer $peer " +
            "creating a new peer instance for every incoming connection")

          val (listener, peerType) = listeners.head

          val listening =
            remoteConnections.listenWithCallback(
                listener, peerType, createDesignatedInstance = true) {
              case Success((remote, remoteConnections)) =>
                runPeer(remoteConnections, Seq(remote), Seq.empty, connectors)
              case Failure(exception) =>
                logging.warn("establishing connection failed", exception)
            }

          listening.failed foreach { exception =>
            logging.error("could not listen for remote instances", exception)
          }
        }
        else {
          logging.info(s"multitier runtime started for peer $peer " +
            "creating a single peer instance")

          remoteConnections.terminated foreach { _ => terminate() }

          if (remoteConnections.isTerminated)
            terminate()
          else
            runPeer(remoteConnections, Seq.empty, listeners, connectors)
        }

        state.run()
      }
      catch {
        case NonFatal(e) =>
          state.terminate()
          doTerminated.set()
          throw e
      }
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

    val requiredConnectedRemotes = requiredConnectors map {
      case (connector, peer) =>
        val reference = remoteConnections.connect(connector, peer)

        reference foreach {
          case Success(_) =>
          case Failure(exception) =>
            logging.error("could not connect to remote instance", exception)
        }

        reference
    }

    val flatInit = Notice.Steady[Try[Seq[Remote.Reference]]]
    flatInit.set(Success(Seq.empty))

    val flatRequiredConnectedRemotes = requiredConnectedRemotes.foldRight(flatInit) {
      case (connected, flattened) =>
        val result = Notice.Steady[Try[Seq[Remote.Reference]]]
        flattened.notice foreach { list =>
          if (list.isSuccess)
            connected foreach { connected =>
              result.set(connected flatMap { connected => list map { connected +: _ } })
            }
          else
            result.set(list)
        }
        result
    }

    listeners foreach { case (listener, peer) =>
      val listening =
        remoteConnections.listenWithCallback(listener, peer) {
          case Success(_) =>
          case Failure(exception) =>
            logging.warn("establishing connection failed", exception)
        }

      listening.failed foreach { exception =>
        logging.warn("could not listen for remote instances", exception)
      }
    }

    flatRequiredConnectedRemotes.notice foreach {
      case Success(requiredConnectedRemotes) =>
        val remotes = optionalConnectors map { case (connector, peer) =>
          val reference = remoteConnections.connect(connector, peer)

          reference foreach {
            case Success(_) =>
            case Failure(exception) =>
              logging.warn("could not connect to remote instance", exception)
          }

          reference
        }

        remoteConnections.terminated foreach { _ =>
          state.synchronized {
            val index = state.instances indexWhere { _.remoteConnections eq remoteConnections }
            if (index != -1)
              state.instances.remove(index)
          }
        }

        context.execute(new Runnable {
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
              doStarted.fire(instance)
              doInstance.trySet(instance)
            }
          }
        })

      case Failure(exception) =>
        context.reportFailure(exception)
        remoteConnections.terminate()
    }
  }

  def terminate(): Unit = state.synchronized {
    if (!state.isTerminated) {
      logging.info("multitier runtime terminated")

      state.terminate()
      state.instances.toSeq foreach { _.terminate() }
      state.instances.clear()

      remoteConnections.terminate()
      doTerminated.set()
    }
  }

  val terminated: Notice.Steady[Unit] = doTerminated.notice

  val started: Notice.Stream[Instance[P]] = doStarted.notice

  val instance: Notice.Steady[Instance[P]] = doInstance.notice

  def instances: Seq[Instance[P]] = state.synchronized {
    state.instances.toList
  }

  @throws(classOf[TimeoutException])
  @throws(classOf[InterruptedException])
  override def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    terminated.ready(atMost)
    this
  }

  @throws(classOf[Exception])
  override def result(atMost: Duration)(implicit permit: CanAwait): Unit =
    terminated.result(atMost)
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
     Seq[Notice.Steady[Try[Remote.Reference]]]) => PlacedValues

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
