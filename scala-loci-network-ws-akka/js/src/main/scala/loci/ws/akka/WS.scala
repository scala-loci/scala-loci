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
  private[akka] def jvmOnly = sys.error(
      "A method of the WebSocket network provider has been called. " +
      "This is most likely because you tried to setup " +
      "the WebSocket network provider in a JavaScript environment. " +
      "The WebSocket network provider only supports the JVM.")

  def apply(
      http: HttpExt, port: Int)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): ConnectionListener =
    jvmOnly
  def apply(
      http: HttpExt, port: Int, interface: String)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): ConnectionListener =
    jvmOnly
  def apply(
      http: HttpExt, port: Int, secured: Boolean)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): ConnectionListener =
    jvmOnly
  def apply(
      http: HttpExt, port: Int, interface: String, secured: Boolean)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): ConnectionListener =
    jvmOnly
  def apply(port: Int): ConnectionListener =
    jvmOnly
  def apply(port: Int, interface: String): ConnectionListener =
    jvmOnly
  def apply(port: Int, secured: Boolean): ConnectionListener =
    jvmOnly
  def apply(port: Int, interface: String, secured: Boolean): ConnectionListener =
    jvmOnly

  def apply(http: HttpExt, webSocketRequest: WebSocketRequest)(
      implicit materializer: Materializer): ConnectionRequestor =
    jvmOnly
  def apply(http: HttpExt, url: Uri)(
      implicit materializer: Materializer): ConnectionRequestor =
    jvmOnly
  def apply(http: HttpExt, url: String)(
      implicit materializer: Materializer): ConnectionRequestor =
    jvmOnly
  def apply(webSocketRequest: WebSocketRequest): ConnectionRequestor =
    jvmOnly
  def apply(url: Uri): ConnectionRequestor =
    jvmOnly
  def apply(url: String): ConnectionRequestor =
    new WSConnectionRequestor(url)

  def createProtocolInfo(
      url: String, host: Option[String], port: Option[Int],
      establisher: ConnectionEstablisher,
      isEncrypted: Boolean, isProtected: Boolean,
      isAuthenticated: Boolean, identification: Option[Any]) =
    new WS(
      url, host, port)(
      establisher, isEncrypted, isProtected, isAuthenticated, identification) { }

  def listener(config: String, attrs: Attributes) =
    None
  def requestor(url: String, attrs: Attributes) =
    if ((url substring (0, 5) compareToIgnoreCase "ws://") == 0 ||
        (url substring (0, 6) compareToIgnoreCase "wss://") == 0)
      Some(WS(url))
    else
      None
}

object WebSocketListener {
  def apply(): ConnectionListener with WebSocketRoute =
    WS.jvmOnly
  def apply(secured: Boolean): ConnectionListener with WebSocketRoute =
    WS.jvmOnly
}

trait WebSocketRoute extends Route {
  def apply(authenticated: Boolean): Route
}
