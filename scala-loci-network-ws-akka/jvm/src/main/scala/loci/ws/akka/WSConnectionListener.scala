package loci
package ws.akka

import network.Connection
import network.ConnectionListener
import contexts.Immediate.Implicits.global
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.http.scaladsl.Http
import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.RequestContext
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.headers._
import scala.concurrent.Future

private object WSConnectionListener {
  private def webSocketRoute(
      establisher: ConnectionListener,
      authenticated: Boolean,
      secured: Boolean)(
      connectionEstablished: Connection => Unit): Route =
    extractUpgradeToWebSocket { webSocket =>
      extractRequest { request =>
        extractMaterializer { implicit materializer =>
          val ip = request.header[`Remote-Address`] flatMap { _.address.toIP }
          val host = ip map { _.ip.getHostName }
          val port = ip flatMap { _.port }

          val SecurityProperties(
              isAuthenticated, isProtected, isEncrypted, certificates) =
            SecurityProperties(Right(request), authenticated)

          if (secured && (!isProtected || !isEncrypted))
            reject
          else
            complete (
              webSocket handleMessages (
                WSConnectionHandler handleWebSocket (
                  Future successful
                    WS.createProtocolInfo(
                      request.uri.toString, host, port,
                      establisher, isEncrypted, isProtected, isAuthenticated,
                      Some((request, certificates))),
                  connectionEstablished, Function const { })))
        }
      }
    }

  def apply(
      http: HttpExt, port: Int, interface: String, secured: Boolean)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer) =
    new DistinctBinding(http, port, interface, secured)

  def apply(port: Int, interface: String, secured: Boolean) =
    new DistinctServer(port, interface, secured)

  def apply(secured: Boolean) =
    new IntegratedRoute(secured)


  class IntegratedRoute(secured: Boolean)
      extends WebSocketRoute with ConnectionListener {
    def route(authenticated: Boolean) =
      webSocketRoute(this, authenticated, secured) {
        doConnectionEstablished(_)
      }

    def apply(authenticated: Boolean) = route(authenticated)

    def apply(v: RequestContext) = route(authenticated = false)(v)
    def start() = { }
    def stop() = { }
  }

  abstract class BoundRoute(secured: Boolean) extends ConnectionListener {
    @volatile private var running: Future[Http.ServerBinding] = _

    def bindRoute(
        http: HttpExt, port: Int, interface: String)(
        connectionEstablished: Connection => Unit)(implicit
        actorRefFactory: ActorSystem,
        materializer: Materializer) = WSActorSystem synchronized {
      if (running == null) {
        running = http bindAndHandle (
          webSocketRoute(
            this, authenticated = false, secured)(
            connectionEstablished),
          interface, port)

        running.failed foreach { _ => stop }
      }
    }

    def start() = WSActorSystem synchronized {
      if (running == null)
        starting
    }

    def stop() = WSActorSystem synchronized {
      if (running != null) {
        stopping
        running foreach { _.unbind }
        running = null
      }
    }

    def starting(): Unit

    def stopping(): Unit
  }

  class DistinctBinding(
    http: HttpExt, port: Int, interface: String, secured: Boolean)(implicit
    actorRefFactory: ActorSystem,
    materializer: Materializer)
      extends BoundRoute(secured) {
    def starting() =
      bindRoute(http, port, interface) { doConnectionEstablished(_) }

    def stopping() = { }
  }

  class DistinctServer(port: Int, interface: String, secured: Boolean)
      extends BoundRoute(secured) {
    def starting() = {
      implicit val (actorSystem, actorMaterializer) = WSActorSystem.retrieve

      bindRoute(Http(), port, interface) { connection =>
        WSActorSystem.retrieve
        connection.closed += { _ => WSActorSystem.release }
        doConnectionEstablished(connection)
      }
    }

    def stopping() = WSActorSystem.release
  }
}
