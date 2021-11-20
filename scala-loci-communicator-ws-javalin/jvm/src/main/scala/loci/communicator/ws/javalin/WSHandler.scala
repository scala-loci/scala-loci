package loci
package communicator
package ws.javalin

import java.util.concurrent.{Executors, ScheduledFuture, ThreadFactory, TimeUnit}

import io.javalin.websocket._

import scala.util.{Success, Try}

private object WSHandler {
  locally(WSHandler)

  private val executor = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
    override def newThread(runnable: Runnable): Thread = {
      val thread = Executors.defaultThreadFactory.newThread(runnable)
      thread.setDaemon(true)
      thread
    }
  })

  val connectionAttributeKey = "loci.communicator.ws.javalin.Connection"

  def connectionAttribute(
      ctx: WsContext,
      connection: Connection[WS],
      doReceive: Notice.Stream.Source[MessageBuffer],
      resetTimeout: () => Unit) =
    ctx.attribute(connectionAttributeKey, (connection, doReceive, resetTimeout))

  def connectionAttribute(ctx: WsContext) =
    ctx.attribute[(Connection[WS], Notice.Stream.Source[MessageBuffer], () => Unit)](connectionAttributeKey)

  def handleConnection(
      wsHandler: WsHandler,
      wsPath: String,
      properties: WS.Properties,
      connectionSetup: ConnectionSetup[WS],
      connectionEstablished: Try[Connection[WS]] => Unit): Unit = {
    wsHandler.onConnect(new WsConnectHandler {
      override def handleConnect(ctx: WsConnectContext): Unit = {
        val connection = synchronized {

          // heartbeat

          val timeout = properties.heartbeatTimeout.toMillis.toInt
          val delay = properties.heartbeatDelay.toMillis
          val heartbeat = "\uD83D\uDC93"

          var heartbeatTask: ScheduledFuture[_] = null

          var timeoutTask: ScheduledFuture[_] = null


          // connection interface

          var isOpen = true
          val doClosed = Notice.Steady[Unit]
          val doReceive = Notice.Stream[MessageBuffer]

          val connection = new Connection[WS] {
            val protocol = new WS {
              val path = wsPath
              val host = None
              val port = None
              val context = ctx
              val setup = connectionSetup
              val authenticated = false
              val encrypted = false
              val integrityProtected = false
            }

            val closed  = doClosed.notice
            val receive = doReceive.notice

            def open: Boolean = synchronized { isOpen }

            def send(data: MessageBuffer) = synchronized {
              if (open)
                ctx.send(data.asByteBuffer)
            }

            def close() = {
              synchronized {
                if (open) {
                  heartbeatTask.cancel(true)
                  timeoutTask.cancel(true)
                  ctx.session.close()

                  isOpen = false
                }
              }

              doClosed.trySet()
            }
          }

          // heartbeat

          heartbeatTask = executor.scheduleWithFixedDelay(new Runnable {
            def run() = connection synchronized {
              ctx.send(heartbeat)
            }
          }, delay, delay, TimeUnit.MILLISECONDS)

          def resetTimeout(): Unit = synchronized {
            if (timeoutTask != null)
              timeoutTask.cancel(true)

            timeoutTask = executor.schedule(new Runnable {
              def run() = connection synchronized {
                connection.close()
              }
            }, timeout, TimeUnit.MILLISECONDS)
          }

          connectionAttribute(ctx, connection, doReceive, resetTimeout _)

          resetTimeout()

          connection
        }

        connectionEstablished(Success(connection))
      }
    })


    // frame parsing

    wsHandler.onMessage(new WsMessageHandler {
      override def handleMessage(ctx: WsMessageContext): Unit = synchronized {
        val (_, _, resetTimeout) = connectionAttribute(ctx)
        resetTimeout()
      }
    })

    wsHandler.onBinaryMessage(new WsBinaryMessageHandler {
      override def handleBinaryMessage(ctx: WsBinaryMessageContext): Unit = {
        val (doReceive, data) = synchronized {
          val (_, doReceive, resetTimeout) = connectionAttribute(ctx)
          resetTimeout()

          if (ctx.offset == 0 && ctx.length == ctx.data.length)
            doReceive -> ctx.data
          else
            doReceive -> ctx.data.slice(ctx.offset, ctx.length)
        }

        doReceive.fire(MessageBuffer.wrapArray(data))
      }
    })

    wsHandler.onClose(new WsCloseHandler {
      override def handleClose(ctx: WsCloseContext): Unit = {
        val connection = synchronized {
          val (connection, _, _) = connectionAttribute(ctx)
          connection
        }
        connection.close()
      }
    })
  }
}
