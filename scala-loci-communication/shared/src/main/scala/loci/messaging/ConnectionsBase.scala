package loci
package messaging

import communicator.ProtocolCommon
import communicator.Bidirectional
import communicator.Connection
import communicator.Connector
import communicator.Listener
import communicator.Listening
import communicator.ConnectionException
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.ReentrantLock

object ConnectionsBase {
  type Protocol = ProtocolCommon with Bidirectional
}

trait ConnectionsBase[R, M] {
  protected def terminatedException =
    new ConnectionException("remote connection terminated")

  protected class BaseState {
    private val running = new AtomicBoolean(false)
    private val terminated = new AtomicBoolean(false)

    def run() = running set true
    def terminate() = terminated set true
    def isRunning = running.get && !terminated.get
    def isTerminated = terminated.get

    private[ConnectionsBase] val messages = new ListBuffer[(R, M)]
    private[ConnectionsBase] val listeners = new ListBuffer[Listening]
    private[ConnectionsBase] val remotes = new ConcurrentLinkedQueue[R]
    private[ConnectionsBase] val connections =
      new ConcurrentHashMap[R, Connection[ConnectionsBase.Protocol]]
  }

  protected val state: BaseState

  private val doRemoteJoined = Notifier[R]

  private val doRemoteLeft = Notifier[R]

  private val doTerminated = Notifier[List[R]]

  private val doReceive = Notifier[(R, M)]

  val remoteJoined: Notification[R] = doRemoteJoined.notification

  val remoteLeft: Notification[R] = doRemoteLeft.notification

  val terminated: Notification[List[R]] = doTerminated.notification

  val receive: Notification[(R, M)] = doReceive.notification

  def remotes: List[R] = state.remotes.asScala.toList

  def isRunning: Boolean = state.isRunning

  def isTerminated: Boolean = state.isTerminated

  def isConnected(remote: R): Boolean =
    state.connections containsKey remote

  def run(): Unit =
    sync {
      if (!state.isTerminated && !state.isRunning) {
        state.messages foreach { doReceive(_) }
        state.messages.clear
        state.run
      }
    }

  def terminate(): Unit =
    sync {
      if (!state.isTerminated) {
        val remotes = state.remotes.asScala.toList
        val connections = state.connections.asScala.toSeq
        val listeners = state.listeners.toSeq

        state.terminate

        afterSync {
          connections foreach { case (_, connection) => connection.close }
          listeners foreach { _.stopListening }
          doTerminated(remotes)

          state.remotes.clear
          state.connections.clear
          state.listeners.clear
        }
      }
    }

  def disconnect(remote: R): Unit =
    if (!state.isTerminated)
      Option(state.connections get remote) foreach { _.close }

  def send(remote: R, message: M): Unit =
    if (!state.isTerminated)
      Option(state.connections get remote) foreach {
        _ send serializeMessage(message)
      }

  private def doBufferedReceive(remote: R, message: M): Unit =
    if (!state.isTerminated) {
      if (state.isRunning)
        doReceive((remote, message))
      else
        sync {
          if (state.isRunning)
            doReceive((remote, message))
          else
            state.messages += remote -> message
        }
    }

  private val syncLock = new ReentrantLock

  private val syncHandlers = new ThreadLocal[ListBuffer[() => Unit]] {
    override def initialValue = ListBuffer.empty
  }

  protected def sync[T](body: => T): T = {
    syncLock.lock
    try body
    finally {
      syncLock.unlock
      if (syncLock.getHoldCount == 0) {
        val handlers = syncHandlers.get
        if (handlers.nonEmpty) {
          val result = handlers.result
          handlers.clear
          result foreach { _.apply }
        }
      }
    }
  }

  protected def afterSync[T](handler: => T): Unit =
    if (syncLock.getHoldCount == 0)
      throw new UnsupportedOperationException(
        "current thread does not hold the synchronization lock")
    else
      syncHandlers.get += handler _


  protected def deserializeMessage(message: MessageBuffer): Try[M]

  protected def serializeMessage(message: M): MessageBuffer


  protected def addListening(listening: Listening): Try[Unit] =
    sync {
      if (!isTerminated) {
        state.listeners += listening
        Success(())
      }
      else
        Failure(terminatedException)
    }

  protected def addConnection(
      remote: R, connection: Connection[ConnectionsBase.Protocol]): Try[Unit] =
    sync {
      if (!isTerminated) {
        state.connections put (remote, connection)
        state.remotes add remote

        afterSync { doRemoteJoined(remote) }

        var receiveHandler: Notifiable[_] = null
        var closedHandler: Notifiable[_] = null

        receiveHandler = connection.receive notify {
          deserializeMessage(_) map { doBufferedReceive(remote, _) }
        }

        closedHandler = connection.closed notify { _ =>
          if (receiveHandler != null)
            receiveHandler.remove
          if (closedHandler != null)
            closedHandler.remove
          removeConnection(remote)
        }

        if (!connection.open) {
          receiveHandler.remove
          closedHandler.remove
          removeConnection(remote)
          Failure(terminatedException)
        }
        else
          Success(())
      }
      else
        Failure(terminatedException)
    }

  protected def removeConnection(remote: R): Unit =
    sync {
      if (state.connections containsKey remote) {
        state.remotes remove remote
        state.connections remove remote
        afterSync { doRemoteLeft(remote) }
      }
    }
}
