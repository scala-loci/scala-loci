package loci.communicator.ws.javalin

import java.util.concurrent.{Executors, ThreadFactory, TimeUnit}

import io.javalin.websocket.{WsBinaryMessageContext, WsBinaryMessageHandler, WsCloseContext, WsCloseHandler, WsConnectContext, WsConnectHandler, WsHandler, WsMessageContext, WsMessageHandler}
import loci.communicator.{Connection, ConnectionSetup}
import loci.{MessageBuffer, Notice}

import scala.util.{Success, Try}

private object WSHandler {
  locally(WSHandler)

  val executor = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
    override def newThread(r: Runnable): Thread = {
      val thr = Executors.defaultThreadFactory().newThread(r)
      thr.setDaemon(true)
      thr
    }
  })


  def handleConnection(
                        wsHandler: WsHandler,
                        wsPath: String,
                        properties: WS.Properties,
                        connectionSetup: ConnectionSetup[WS],
                        connectionEstablished: Try[Connection[WS]] => Unit): Unit = {



    val doClosed  = Notice.Steady[Unit]
    val doReceive = Notice.Stream[MessageBuffer]

    var connection : Connection[WS] = null

    wsHandler.onConnect(new WsConnectHandler {
      override def handleConnect(wsCtx: WsConnectContext): Unit = {


        // heartbeat
        val timeout   = properties.heartbeatTimeout.toMillis.toInt
        val delay     = properties.heartbeatDelay.toMillis
        val heartbeat = "\uD83D\uDC93"

        wsCtx.session.setIdleTimeout(timeout)

        val heartbeatTask = executor.scheduleWithFixedDelay(new Runnable {
          def run = {
            wsCtx.send(heartbeat)
          }
        }, delay, delay, TimeUnit.MILLISECONDS)

        // connection interface

        var isOpen     = true
        val socketLock = new Object()


        connection = new Connection[WS] {

          val protocol = new WS {
            val path               = wsPath
            val setup              = connectionSetup
            val authenticated      = false
            val encrypted          = false
            val integrityProtected = false
          }

          val closed  = doClosed.notice
          val receive = doReceive.notice

          override def open: Boolean = socketLock.synchronized(isOpen)

          def send(data: MessageBuffer) = socketLock.synchronized {
            if (open) {
              wsCtx.send(data.asByteBuffer)
            }
          }

          def close() = socketLock.synchronized {
            if (open) {

              heartbeatTask.cancel(true)
              wsCtx.session.close()

              isOpen = false
              doClosed.set()
            }
          }
        }

        connectionEstablished(Success(connection))

      }
    })

    // frame parsing

    wsHandler.onMessage(new WsMessageHandler {
      override def handleMessage(ctx: WsMessageContext): Unit = () //heartbeat
    })

    wsHandler.onBinaryMessage(new WsBinaryMessageHandler {
      override def handleBinaryMessage(ctx: WsBinaryMessageContext): Unit = {
        doReceive.fire(MessageBuffer.wrapArray(ctx.data()))
      }
    })

    wsHandler.onClose(new WsCloseHandler {
      override def handleClose(ctx: WsCloseContext): Unit =
        if(connection!= null) connection.close()
    })
  }
}
