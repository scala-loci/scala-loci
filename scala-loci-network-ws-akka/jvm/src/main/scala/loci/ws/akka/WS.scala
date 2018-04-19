package loci
package ws.akka

import network.ConnectionEstablisher
import network.ConnectionListener
import network.ConnectionRequestor
import network.ConnectionFactory
import network.ProtocolInfo
import util.Attributes
import akka.actor.ActorSystem
import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.ws.WebSocketRequest
import akka.stream.Materializer

abstract case class WS private[WS] (
    url: String, host: Option[String], port: Option[Int])(
    val establisher: ConnectionEstablisher,
    val isEncrypted: Boolean, val isProtected: Boolean,
    val isAuthenticated: Boolean, val identification: Option[Any])
  extends ProtocolInfo {

  private def readResolve(): Object =
    WS.createProtocolInfo(
      url, host, port, establisher,
      isEncrypted, isProtected, isAuthenticated, identification)
  def copy(
      url: String = url,
      host: Option[String] = host,
      port: Option[Int] = port): WS =
    WS.createProtocolInfo(
      url, host, port, establisher,
      isEncrypted, isProtected, isAuthenticated, identification)
}

object WS extends ConnectionFactory {
  def apply(
      http: HttpExt, port: Int)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): ConnectionListener =
    WSConnectionListener(http, port, "localhost", secured = false)
  def apply(
      http: HttpExt, port: Int, interface: String)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): ConnectionListener =
    WSConnectionListener(http, port, interface, secured = false)
  def apply(
      http: HttpExt, port: Int, secured: Boolean)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): ConnectionListener =
    WSConnectionListener(http, port, "localhost", secured)
  def apply(
      http: HttpExt, port: Int, interface: String, secured: Boolean)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): ConnectionListener =
    WSConnectionListener(http, port, interface, secured)
  def apply(port: Int): ConnectionListener =
    WSConnectionListener(port, "localhost", secured = false)
  def apply(port: Int, interface: String): ConnectionListener =
    WSConnectionListener(port, interface, secured = false)
  def apply(port: Int, secured: Boolean): ConnectionListener =
    WSConnectionListener(port, "localhost", secured)
  def apply(port: Int, interface: String, secured: Boolean): ConnectionListener =
    WSConnectionListener(port, interface, secured)

  def apply(http: HttpExt, webSocketRequest: WebSocketRequest)(
      implicit materializer: Materializer): ConnectionRequestor =
    WSConnectionRequestor(http, webSocketRequest)
  def apply(http: HttpExt, url: Uri)(
      implicit materializer: Materializer): ConnectionRequestor =
    WSConnectionRequestor(http, WebSocketRequest(url))
  def apply(http: HttpExt, url: String)(
      implicit materializer: Materializer): ConnectionRequestor =
    WSConnectionRequestor(http, WebSocketRequest(url))
  def apply(webSocketRequest: WebSocketRequest): ConnectionRequestor =
    WSConnectionRequestor(webSocketRequest)
  def apply(url: Uri): ConnectionRequestor =
    WSConnectionRequestor(WebSocketRequest(url))
  def apply(url: String): ConnectionRequestor =
    WSConnectionRequestor(WebSocketRequest(url))

  def createProtocolInfo(
      url: String, host: Option[String], port: Option[Int],
      establisher: ConnectionEstablisher,
      isEncrypted: Boolean, isProtected: Boolean,
      isAuthenticated: Boolean, identification: Option[Any]) =
    new WS(
      url, host, port)(
      establisher, isEncrypted, isProtected, isAuthenticated, identification) { }

  def listener(config: String, attrs: Attributes) =
    WSConnectionFactory listener (config, attrs)
  def requestor(url: String, attrs: Attributes) =
    WSConnectionFactory requestor (url, attrs)
}

object WebSocketListener {
  def apply(): ConnectionListener with WebSocketRoute =
    WSConnectionListener(secured = false)
  def apply(secured: Boolean): ConnectionListener with WebSocketRoute =
    WSConnectionListener(secured)
}

trait WebSocketRoute extends Route {
  def apply(authenticated: Boolean): Route
}
