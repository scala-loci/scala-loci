package loci
package communicator
package ws.jetty

import org.eclipse.jetty.websocket.api.{Callback, Session}
import org.eclipse.jetty.websocket.api.Session.Listener

import java.nio.ByteBuffer
import java.util.concurrent.{Executors, RejectedExecutionException, ScheduledFuture, ThreadFactory, TimeUnit}
import scala.util.{Failure, Success}

private class Socket[P <: WS: WSProtocolFactory](
  val protocol: P,
  properties: WS.Properties)(
  connectionEstablished: Success[Connection[P]] => Unit,
  connectionFailed: Failure[Connection[P]] => Unit) extends Listener.Abstract with Connection[P] {

  val doClosed = Notice.Steady[Unit]
  val doReceive = Notice.Stream[MessageBuffer]

  private val executor = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
    def newThread(runnable: Runnable) = {
      val thread = Executors.defaultThreadFactory.newThread(runnable)
      thread.setDaemon(true)
      thread
    }
  })

  private val timeout = properties.heartbeatTimeout.toMillis
  private val delay = properties.heartbeatDelay.toMillis
  private val heartbeat = "\uD83D\uDC93"

  private var heartbeatTask: ScheduledFuture[_] = _

  private var timeoutTask: ScheduledFuture[_] = _

  private def resetTimeout(): Unit = synchronized {
    if (timeoutTask != null)
      timeoutTask.cancel(true)

    timeoutTask =
      try
        executor.schedule(new Runnable {
          def run(): Unit = Socket.this synchronized {
            getSession.close()
          }
        }, timeout, TimeUnit.MILLISECONDS)
      catch {
        case _: RejectedExecutionException if executor.isShutdown =>
          null
      }
  }

  resetTimeout()

  override def onWebSocketOpen(session: Session): Unit = {
    synchronized {
      super.onWebSocketOpen(session)

      heartbeatTask =
        try
          executor.scheduleWithFixedDelay(new Runnable {
            def run(): Unit = Socket.this synchronized {
              getSession.sendText(heartbeat, Callback.NOOP)
            }
          }, delay, delay, TimeUnit.MILLISECONDS)
        catch {
          case _: RejectedExecutionException if executor.isShutdown =>
            null
        }
    }

    connectionEstablished(Success(this))

    resetTimeout()
    session.demand()
  }

  override def onWebSocketBinary(buffer: ByteBuffer, callback: Callback): Unit = {
    val data = new Array[Byte](buffer.remaining)
    buffer.get(data)

    doReceive.fire(MessageBuffer.wrapArray(data))

    resetTimeout()
    callback.succeed()
    getSession.demand()
  }

  override def onWebSocketText(message: String): Unit = {
    synchronized { super.onWebSocketText(message) }
    resetTimeout()
    getSession.demand()
  }

  override def onWebSocketClose(statusCode: Int, reason: String): Unit = {
    synchronized {
      super.onWebSocketClose(statusCode, reason)
      if (heartbeatTask != null)
        heartbeatTask.cancel(true)
      if (timeoutTask != null)
        timeoutTask.cancel(true)
    }

    executor.shutdown()
    doClosed.trySet()
  }

  override def onWebSocketError(cause: Throwable): Unit = {
    synchronized { super.onWebSocketError(cause) }
    connectionFailed(Failure(cause))
    close()
  }

  val closed = doClosed.notice
  val receive = doReceive.notice

  def open: Boolean = synchronized {
    val session = getSession
    session != null && session.isOpen
  }

  def send(data: MessageBuffer): Unit = synchronized {
    if (isOpen)
      getSession.sendBinary(data.asByteBuffer, Callback.NOOP)
  }

  def close(): Unit = {
    synchronized {
      val session = getSession
      if (session != null)
        session.close()
    }

    executor.shutdown()
    doClosed.trySet()
  }
}
