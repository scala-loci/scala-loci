package loci.communicator.ws.javalin

import java.util.function.Consumer

import io.javalin.Javalin
import io.javalin.websocket.WsHandler
import loci.communicator
import loci.communicator.{Bidirectional, Connector, Listener, Listening, Protocol, SecurityInfo, SetupInfo, SymmetryInfo}

import scala.concurrent.duration._
import scala.util.{Success, Try}

trait WS extends
         Protocol with
         SetupInfo with
         SecurityInfo with
         SymmetryInfo with Bidirectional {
  val path: String
  val host: Option[String] = None
  val port: Option[Int]    = None

  override def toString = s"JavalinWS($path)"
}

object WS extends WSSetupFactory {
  def unapply(ws: WS) = Some((ws.path, ws.host, ws.port))

  case class Properties(heartbeatDelay: FiniteDuration = 3.seconds,
                        heartbeatTimeout: FiniteDuration = 10.seconds)

  def apply(javalin: Javalin, path: String): Listener[WS] =
    apply(javalin, path, Properties())

  def apply(javalin: Javalin, path: String, properties: Properties): Listener[WS] =
    new Listener[WS] {
      self =>
      protected def startListening(connectionEstablished: Connected[WS]): Try[Listening] = {
        javalin.ws(path, new Consumer[WsHandler] {
          override def accept(ws: WsHandler): Unit =
            WSHandler.handleConnection(ws, path, properties, self, connectionEstablished.fire)
        })
        Success(new Listening {
          def stopListening(): Unit = ()
        })
      }
    }


  def apply(url: String): Connector[WS] = ???
  def apply(url: String, properties: Properties): Connector[WS] = ???

  trait Secure extends WS with communicator.Secure

  object Secure {
    def unapply(ws: Secure) = Some((ws.path, ws.host, ws.port))

    def apply(url: String): Connector[WS.Secure] = ???
    def apply(url: String, properties: Properties): Connector[WS.Secure] = ???
  }
}
