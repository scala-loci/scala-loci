package loci
package communicator
package ws.javalin

import java.util.function.Consumer

import io.javalin.Javalin
import io.javalin.websocket.{WsContext, WsConfig}

import scala.concurrent.duration._
import scala.util.{Success, Try}

trait WS extends
    Protocol with
    SetupInfo with
    SecurityInfo with
    SymmetryInfo with Bidirectional {
  val path: String
  val host: Option[String]
  val port: Option[Int]
  val context: WsContext

  override def toString = s"WS($path, $host, $port)"
}

object WS extends WSSetupFactory {
  def unapply(ws: WS) = Some((ws.path, ws.host, ws.port))

  case class Properties(
    heartbeatDelay: FiniteDuration = 3.seconds,
    heartbeatTimeout: FiniteDuration = 10.seconds)

  def apply(javalin: Javalin, path: String): Listener[WS] =
    apply(javalin, path, Properties())

  def apply(javalin: Javalin, path: String, properties: Properties): Listener[WS] =
    new Listener[WS] { self =>
      protected def startListening(connectionEstablished: Connected[WS]): Try[Listening] = {
        javalin.ws(path, new Consumer[WsConfig] {
          override def accept(ws: WsConfig): Unit =
            WSHandler.handleConnection(ws, path, properties, self, connectionEstablished.fire)
        })

        Success(new Listening {
          def stopListening(): Unit = ()
        })
      }
    }
}
