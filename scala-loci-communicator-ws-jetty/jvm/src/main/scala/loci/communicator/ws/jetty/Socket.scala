package loci
package communicator
package ws.jetty

import org.eclipse.jetty.websocket.api.{Session, WebSocketAdapter}

import java.util.concurrent.{Executors, ScheduledFuture, ThreadFactory, TimeUnit}
import scala.util.{Failure, Success, Try}

class Socket[P <: WS: WSProtocolFactory](
    override val protocol: P,
    properties: WS.Properties,
    onError: Try[Connection[P]] => Unit,
    connectionEstablished: Try[Connection[P]] => Unit
) extends WebSocketAdapter with Connection[P] {

  val doClosed  = Notice.Steady[Unit]
  val doReceive = Notice.Stream[MessageBuffer]

  private val executor = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
    def newThread(runnable: Runnable) = {
      val thread = Executors.defaultThreadFactory.newThread(runnable)
      thread.setDaemon(true)
      thread
    }
  })

  private val timeout: Int = properties.heartbeatTimeout.toMillis.toInt
  private val delay: Long  = properties.heartbeatDelay.toMillis
  private val heartbeat    = "\uD83D\uDC93"

  private var heartbeatTask: ScheduledFuture[_] = _

  private var timeoutTask: ScheduledFuture[_] = _

  private def resetTimeout(): Unit = {
    if (timeoutTask != null)
      timeoutTask.cancel(true)

    timeoutTask = executor.schedule(
      new Runnable {
        override def run(): Unit = getSession.close()
      },
      timeout,
      TimeUnit.MILLISECONDS
    )
  }

  resetTimeout()

  override def onWebSocketConnect(sess: Session): Unit = {
    super.onWebSocketConnect(sess)

    connectionEstablished(Success(this))

    heartbeatTask = executor.scheduleWithFixedDelay(
      new Runnable { def run(): Unit = getRemote.sendString(heartbeat) },
      delay,
      delay,
      TimeUnit.MILLISECONDS
    )

    resetTimeout()
  }

  override def onWebSocketBinary(payload: Array[Byte], offset: Int, len: Int): Unit = {
    super.onWebSocketBinary(payload, offset, len)

    doReceive.fire(MessageBuffer.wrapArray(payload.drop(offset)))

    resetTimeout()
  }

  override def onWebSocketText(message: String): Unit = {
    super.onWebSocketText(message)
    // TODO: strangely enough … text messages are not handled?
    resetTimeout()
  }

  override def onWebSocketClose(statusCode: Int, reason: String): Unit = {
    super.onWebSocketClose(statusCode, reason)

    heartbeatTask.cancel(true)
    timeoutTask.cancel(true)
    doClosed.set()
  }

  override def onWebSocketError(cause: Throwable): Unit = {
    loci.logging.warn(s"connection failed ${cause}")
    cause.printStackTrace()
    super.onWebSocketError(cause)

    onError(Failure(cause))
    doClosed.set()

    if (getSession != null) getSession.close()
  }

  val closed  = doClosed.notice
  val receive = doReceive.notice

  override def open: Boolean = {
    getSession != null && getSession.isOpen
  }

  def send(data: MessageBuffer): Unit = {
    if (isConnected) {
      getRemote.sendBytes(data.asByteBuffer)
    }
  }

  def close(): Unit = {
    if (getSession != null) getSession.close()
  }

}
