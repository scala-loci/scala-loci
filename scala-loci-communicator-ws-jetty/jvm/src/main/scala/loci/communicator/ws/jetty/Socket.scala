package loci
package communicator
package ws.jetty

import org.eclipse.jetty.websocket.api.{Session, WebSocketAdapter}

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

  private def resetTimeout(): Unit = {
    if (timeoutTask != null)
      timeoutTask.cancel(true)

    timeoutTask = executor.schedule(
      new Runnable { def run(): Unit = getSession.close() },
      timeout,
      TimeUnit.MILLISECONDS)
  }

  resetTimeout()

  override def onWebSocketConnect(sess: Session): Unit = {
    super.onWebSocketConnect(sess)

    connectionEstablished(Success(this))

    heartbeatTask = executor.scheduleWithFixedDelay(
      new Runnable { def run(): Unit = getRemote.sendString(heartbeat) },
      delay,
      delay,
      TimeUnit.MILLISECONDS)

    resetTimeout()
  }

  override def onWebSocketBinary(payload: Array[Byte], offset: Int, len: Int): Unit = {
    super.onWebSocketBinary(payload, offset, len)

    val data =
      if (offset == 0 && len == payload.length)
        payload
      else
        payload.slice(offset, len)

    doReceive.fire(MessageBuffer.wrapArray(data))

    resetTimeout()
  }

  override def onWebSocketText(message: String): Unit = {
    super.onWebSocketText(message)
    resetTimeout()
  }

  override def onWebSocketClose(statusCode: Int, reason: String): Unit = {
    super.onWebSocketClose(statusCode, reason)

    heartbeatTask.cancel(true)
    timeoutTask.cancel(true)

    doClosed.trySet()
  }

  override def onWebSocketError(cause: Throwable): Unit = {
    super.onWebSocketError(cause)
    connectionFailed(Failure(cause))
    close()
  }

  val closed  = doClosed.notice
  val receive = doReceive.notice

  def open: Boolean = {
    val session = getSession
    session != null && session.isOpen
  }

  def send(data: MessageBuffer): Unit =
    if (isConnected)
      getRemote.sendBytes(data.asByteBuffer)

  def close(): Unit = {
    val session = getSession
    if (session != null)
      session.close()

    doClosed.trySet()
  }
}
