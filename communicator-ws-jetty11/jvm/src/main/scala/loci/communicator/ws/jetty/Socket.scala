package loci
package communicator
package ws.jetty

import org.eclipse.jetty.websocket.api.{Session, WebSocketAdapter, WriteCallback}

import java.util.concurrent.{Executors, ScheduledFuture, ThreadFactory, TimeUnit}
import scala.util.{Failure, Success}

private class Socket[P <: WS: WSProtocolFactory](
  val protocol: P,
  properties: WS.Properties)(
  connectionEstablished: Success[Connection[P]] => Unit,
  connectionFailed: Failure[Connection[P]] => Unit) extends WebSocketAdapter with Connection[P] {

  val doClosed  = Notice.Steady[Unit]
  val doReceive = Notice.Stream[MessageBuffer]

  private val executor = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
    def newThread(runnable: Runnable) = {
      val thread = Executors.defaultThreadFactory.newThread(runnable)
      thread.setDaemon(true)
      thread
    }
  })

  private val timeout = properties.heartbeatTimeout.toMillis
  private val delay  = properties.heartbeatDelay.toMillis
  private val heartbeat = "\uD83D\uDC93"

  private var heartbeatTask: ScheduledFuture[_] = _

  private var timeoutTask: ScheduledFuture[_] = _

  private def resetTimeout(): Unit = synchronized {
    if (timeoutTask != null)
      timeoutTask.cancel(true)

    timeoutTask = executor.schedule(new Runnable {
      def run(): Unit = Socket.this synchronized {
        getSession.close()
      }
    }, timeout, TimeUnit.MILLISECONDS)
  }

  resetTimeout()

  override def onWebSocketConnect(session: Session): Unit = {
    synchronized {
      super.onWebSocketConnect(session)

      heartbeatTask = executor.scheduleWithFixedDelay(new Runnable {
        def run(): Unit = Socket.this synchronized {
          getRemote.sendString(heartbeat, WriteCallback.NOOP)
        }
      }, delay, delay, TimeUnit.MILLISECONDS)
    }

    connectionEstablished(Success(this))

    resetTimeout()
  }

  override def onWebSocketBinary(payload: Array[Byte], offset: Int, len: Int): Unit = {
    val data = synchronized {
      super.onWebSocketBinary(payload, offset, len)

      if (offset == 0 && len == payload.length)
        payload
      else
        payload.slice(offset, len)
    }

    doReceive.fire(MessageBuffer.wrapArray(data))

    resetTimeout()
  }

  override def onWebSocketText(message: String): Unit = {
    synchronized { super.onWebSocketText(message) }
    resetTimeout()
  }

  override def onWebSocketClose(statusCode: Int, reason: String): Unit = {
    synchronized {
      super.onWebSocketClose(statusCode, reason)
      heartbeatTask.cancel(true)
      timeoutTask.cancel(true)
    }

    doClosed.trySet()
  }

  override def onWebSocketError(cause: Throwable): Unit = {
    synchronized { super.onWebSocketError(cause) }
    connectionFailed(Failure(cause))
    close()
  }

  val closed  = doClosed.notice
  val receive = doReceive.notice

  def open: Boolean = synchronized {
    val session = getSession
    session != null && session.isOpen
  }

  def send(data: MessageBuffer): Unit = synchronized {
    if (isConnected)
      getRemote.sendBytes(data.asByteBuffer, WriteCallback.NOOP)
  }

  def close(): Unit = {
    synchronized {
      val session = getSession
      if (session != null)
        session.close()
    }

    doClosed.trySet()
  }
}
