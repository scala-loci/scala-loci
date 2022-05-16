package loci
package communicator
package ws.akka

import akka.actor.ActorSystem
import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.ws.WebSocketRequest
import akka.http.scaladsl.server.Route
import akka.stream.Materializer

import scala.concurrent.duration._

trait WS
    extends Protocol
    with SetupInfo
    with SecurityInfo
    with SymmetryInfo with Bidirectional {
  val url: String
  val host: Option[String]
  val port: Option[Int]

  override def toString = s"WS($url, $host, $port)"
}

object WS extends WSSetupFactory {
  def unapply(ws: WS) = Some((ws.url, ws.host, ws.port))

  case class Properties(
    heartbeatDelay: FiniteDuration = 3.seconds,
    heartbeatTimeout: FiniteDuration = 10.seconds)

  def apply(
      http: HttpExt, port: Int)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): Listener[WS] =
    WSListener[WS](http, port, "localhost", Properties())
  def apply(
      http: HttpExt, port: Int, interface: String)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): Listener[WS] =
    WSListener[WS](http, port, interface, Properties())
  def apply(
      http: HttpExt, port: Int, properties: Properties)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): Listener[WS] =
    WSListener[WS](http, port, "localhost", properties)
  def apply(
      http: HttpExt, port: Int, interface: String,
      properties: Properties)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): Listener[WS] =
    WSListener[WS](http, port, interface, properties)
  def apply(port: Int): Listener[WS] =
    WSListener[WS](port, "localhost", Properties())
  def apply(port: Int, interface: String): Listener[WS] =
    WSListener[WS](port, interface, Properties())
  def apply(port: Int, properties: Properties): Listener[WS] =
    WSListener[WS](port, "localhost", properties)
  def apply(port: Int, interface: String,
      properties: Properties): Listener[WS] =
    WSListener[WS](port, interface, properties)

  def apply(http: HttpExt, webSocketRequest: WebSocketRequest)(
      implicit materializer: Materializer): Connector[WS] =
    WSConnector[WS](http, webSocketRequest, Properties())
  def apply(http: HttpExt, url: Uri)(
      implicit materializer: Materializer): Connector[WS] =
    WSConnector[WS](http, WebSocketRequest(url), Properties())
  def apply(http: HttpExt, url: String)(
      implicit materializer: Materializer): Connector[WS] =
    WSConnector[WS](http, WebSocketRequest(url), Properties())
  def apply(webSocketRequest: WebSocketRequest): Connector[WS] =
    WSConnector[WS](webSocketRequest, Properties())
  def apply(url: Uri): Connector[WS] =
    WSConnector[WS](WebSocketRequest(url), Properties())
  def apply(url: String): Connector[WS] =
    WSConnector[WS](WebSocketRequest(url), Properties())
  def apply(http: HttpExt, webSocketRequest: WebSocketRequest,
        properties: Properties)(
      implicit materializer: Materializer): Connector[WS] =
    WSConnector[WS](http, webSocketRequest, properties)
  def apply(http: HttpExt, url: Uri, properties: Properties)(
      implicit materializer: Materializer): Connector[WS] =
    WSConnector[WS](http, WebSocketRequest(url), properties)
  def apply(http: HttpExt, url: String, properties: Properties)(
      implicit materializer: Materializer): Connector[WS] =
    WSConnector[WS](http, WebSocketRequest(url), properties)
  def apply(webSocketRequest: WebSocketRequest,
      properties: Properties): Connector[WS] =
    WSConnector[WS](webSocketRequest, properties)
  def apply(url: Uri, properties: Properties): Connector[WS] =
    WSConnector[WS](WebSocketRequest(url), properties)
  def apply(url: String, properties: Properties): Connector[WS] =
    WSConnector[WS](WebSocketRequest(url), properties)

  trait Secure extends WS with communicator.Secure {
    override def toString = s"WS.Secure($url, $host, $port)"
  }

  object Secure {
    def unapply(ws: Secure) = Some((ws.url, ws.host, ws.port))

    def apply(
        http: HttpExt, port: Int)(implicit
        actorRefFactory: ActorSystem,
        materializer: Materializer): Listener[WS.Secure] =
      WSListener[WS.Secure](http, port, "localhost", Properties())
    def apply(
        http: HttpExt, port: Int, interface: String)(implicit
        actorRefFactory: ActorSystem,
        materializer: Materializer): Listener[WS.Secure] =
      WSListener[WS.Secure](http, port, interface, Properties())
    def apply(
        http: HttpExt, port: Int, properties: Properties)(implicit
        actorRefFactory: ActorSystem,
        materializer: Materializer): Listener[WS.Secure] =
      WSListener[WS.Secure](http, port, "localhost", properties)
    def apply(
        http: HttpExt, port: Int, interface: String,
        properties: Properties)(implicit
        actorRefFactory: ActorSystem,
        materializer: Materializer): Listener[WS.Secure] =
      WSListener[WS.Secure](http, port, interface, properties)
    def apply(port: Int): Listener[WS.Secure] =
      WSListener[WS.Secure](port, "localhost", Properties())
    def apply(port: Int, interface: String): Listener[WS.Secure] =
      WSListener[WS.Secure](port, interface, Properties())
    def apply(port: Int, properties: Properties): Listener[WS.Secure] =
      WSListener[WS.Secure](port, "localhost", properties)
    def apply(port: Int, interface: String,
        properties: Properties): Listener[WS.Secure] =
      WSListener[WS.Secure](port, interface, properties)

    def apply(http: HttpExt, webSocketRequest: WebSocketRequest)(
        implicit materializer: Materializer): Connector[WS.Secure] =
      WSConnector[WS.Secure](http, webSocketRequest, Properties())
    def apply(http: HttpExt, url: Uri)(
        implicit materializer: Materializer): Connector[WS.Secure] =
      WSConnector[WS.Secure](http, WebSocketRequest(url), Properties())
    def apply(http: HttpExt, url: String)(
        implicit materializer: Materializer): Connector[WS.Secure] =
      WSConnector[WS.Secure](http, WebSocketRequest(url), Properties())
    def apply(webSocketRequest: WebSocketRequest): Connector[WS.Secure] =
      WSConnector[WS.Secure](webSocketRequest, Properties())
    def apply(url: Uri): Connector[WS.Secure] =
      WSConnector[WS.Secure](WebSocketRequest(url), Properties())
    def apply(url: String): Connector[WS.Secure] =
      WSConnector[WS.Secure](WebSocketRequest(url), Properties())
    def apply(http: HttpExt, webSocketRequest: WebSocketRequest,
          properties: Properties)(
        implicit materializer: Materializer): Connector[WS.Secure] =
      WSConnector[WS.Secure](http, webSocketRequest, properties)
    def apply(http: HttpExt, url: Uri, properties: Properties)(
        implicit materializer: Materializer): Connector[WS.Secure] =
      WSConnector[WS.Secure](http, WebSocketRequest(url), properties)
    def apply(http: HttpExt, url: String, properties: Properties)(
        implicit materializer: Materializer): Connector[WS.Secure] =
      WSConnector[WS.Secure](http, WebSocketRequest(url), properties)
    def apply(webSocketRequest: WebSocketRequest,
        properties: Properties): Connector[WS.Secure] =
      WSConnector[WS.Secure](webSocketRequest, properties)
    def apply(url: Uri, properties: Properties): Connector[WS.Secure] =
      WSConnector[WS.Secure](WebSocketRequest(url), properties)
    def apply(url: String, properties: Properties): Connector[WS.Secure] =
      WSConnector[WS.Secure](WebSocketRequest(url), properties)
  }
}

object WebSocketListener {
  def apply(): Listener[WS] with WebSocketRoute =
    WSListener[WS](WS.Properties())
  def apply(properties: WS.Properties): Listener[WS] with WebSocketRoute =
    WSListener[WS](properties)

  object Secure {
    def apply(): Listener[WS.Secure] with WebSocketRoute =
      WSListener[WS.Secure](WS.Properties())
    def apply(properties: WS.Properties): Listener[WS.Secure] with WebSocketRoute =
      WSListener[WS.Secure](properties)
  }
}

trait WebSocketRoute extends Route {
  def apply(authenticatedName: String): Route
  def apply(authenticatedName: Option[String]): Route
}
