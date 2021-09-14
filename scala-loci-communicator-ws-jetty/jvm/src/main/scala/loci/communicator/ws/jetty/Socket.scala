package loci
package communicator
package ws.jetty

import org.eclipse.jetty.websocket.api.{Session, WebSocketAdapter}

import java.util.concurrent.{Executors, ScheduledFuture, TimeUnit}
import scala.util.{Failure, Try}

class Socket[P <: WS : WSProtocolFactory](
  properties: WS.Properties,
  doConnect: Notice.Steady.NoticeSource[Unit],
  doReceive: Notice.Stream.NoticeSource[MessageBuffer],
  doClosed: Notice.Steady.NoticeSource[Unit],
  onError: Try[Connection[P]] => Unit
) extends WebSocketAdapter {

  private val executor = Executors.newSingleThreadScheduledExecutor((runnable: Runnable) => {
    val thread = Executors.defaultThreadFactory.newThread(runnable)
    thread.setDaemon(true)
    thread
  })

  private val timeout: Int = properties.heartbeatTimeout.toMillis.toInt
  private val delay: Long = properties.heartbeatDelay.toMillis
  private val heartbeat = "\uD83D\uDC93"

  private var heartbeatTask: ScheduledFuture[_] = _

  private var timeoutTask: ScheduledFuture[_] = _

  private def resetTimeout(): Unit = {
    if (timeoutTask != null)
      timeoutTask.cancel(true)

    timeoutTask = executor.schedule(new Runnable {
      override def run(): Unit = getSession.close()
    }, timeout, TimeUnit.MILLISECONDS)
  }

  resetTimeout()

  override def onWebSocketConnect(sess: Session): Unit = {
    super.onWebSocketConnect(sess)

    doConnect.set()

    heartbeatTask = executor.scheduleWithFixedDelay(
      () => getRemote.sendString(heartbeat),
      delay, delay, TimeUnit.MILLISECONDS)

    resetTimeout()
  }

  override def onWebSocketBinary(payload: Array[Byte], offset: Int, len: Int): Unit = {
    super.onWebSocketBinary(payload, offset, len)

    doReceive.fire(MessageBuffer.wrapArray(payload.drop(offset)))

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
    doClosed.set()
  }

  override def onWebSocketError(cause: Throwable): Unit = {
    super.onWebSocketError(cause)

    onError(Failure(new ConnectionException("WebSocket failed to connect")))

    if (getSession != null) getSession.close()
  }
}
