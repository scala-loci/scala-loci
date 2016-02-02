package retier
package ws.akka

import network.ConnectionListener
import network.ConnectionRequestor
import network.ConnectionFactory
import network.ProtocolInfo
import util.Attributes
import akka.actor.ActorSystem
import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.ws.WebsocketRequest
import akka.stream.Materializer

abstract case class WS private[WS] (
    url: String, host: Option[String], port: Option[Int])(
    val isEncrypted: Boolean, val isProtected: Boolean, val isAuthenticated: Boolean)
  extends ProtocolInfo {

  private def readResolve(): Object =
    WS.createProtocolInfo(
      url, host, port, isEncrypted, isProtected, isAuthenticated)
  def copy(
      url: String = url,
      host: Option[String] = host,
      port: Option[Int] = port): WS =
    WS.createProtocolInfo(
      url, host, port, isEncrypted, isProtected, isAuthenticated)

  val identification = None
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

  def apply(http: HttpExt, websocketRequest: WebsocketRequest)(
      implicit materializer: Materializer): ConnectionRequestor =
    jvmOnly
  def apply(http: HttpExt, url: Uri)(
      implicit materializer: Materializer): ConnectionRequestor =
    jvmOnly
  def apply(http: HttpExt, url: String)(
      implicit materializer: Materializer): ConnectionRequestor =
    jvmOnly
  def apply(websocketRequest: WebsocketRequest): ConnectionRequestor =
    jvmOnly
  def apply(url: Uri): ConnectionRequestor =
    jvmOnly
  def apply(url: String): ConnectionRequestor =
    jvmOnly

  def createProtocolInfo(
      url: String, host: Option[String], port: Option[Int],
      isEncrypted: Boolean, isProtected: Boolean, isAuthenticated: Boolean) =
    new WS(url, host, port)(isEncrypted, isProtected, isAuthenticated) { }

  def listener(config: String, attrs: Attributes) =
    None
  def requestor(url: String, attrs: Attributes) =
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
