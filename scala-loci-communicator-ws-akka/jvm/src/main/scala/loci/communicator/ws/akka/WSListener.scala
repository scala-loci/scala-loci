package loci
package communicator
package ws.akka

import java.util.concurrent.ConcurrentLinkedQueue

import akka.actor.ActorSystem
import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model.headers
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{RequestContext, Route}
import akka.stream.Materializer

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

private object WSListener {
  locally(WSListener)

  private def webSocketRoute[P <: WS: WSProtocolFactory](
      listener: Listener[P],
      authenticatedName: Option[String],
      properties: WS.Properties)(
      connectionEstablished: Try[Connection[P]] => Unit): Route =
    extractUpgradeToWebSocket { webSocket =>
      extractRequest { request =>
        extractMaterializer { implicit materializer =>
          val ip = request.header[headers.`Remote-Address`] flatMap { _.address.toIP }

          val WSSecurityProperties(isAuthenticated, isProtected, isEncrypted, certificates) =
            WSSecurityProperties(request, authenticatedName.nonEmpty)

          implicitly[WSProtocolFactory[P]] make (
              request.uri.toString,
              ip map { _.ip.getHostName },
              ip flatMap { _.port },
              listener, isAuthenticated, isEncrypted, isProtected,
              Some(Left(request)),
              authenticatedName toRight certificates) match {
            case Failure(exception) =>
              connectionEstablished(Failure(exception))
              reject

            case Success(ws) =>
              complete(
                webSocket.handleMessages(
                  WSHandler.handleWebSocket(
                    Future successful ws, properties, connectionEstablished)))
          }
        }
      }
    }

  def apply[P <: WS: WSProtocolFactory](
      http: HttpExt, port: Int, interface: String,
      properties: WS.Properties)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer) =
    new DistinctBinding(http, port, interface, properties)

  def apply[P <: WS: WSProtocolFactory](
      port: Int, interface: String, properties: WS.Properties) =
    new DistinctServer(port, interface, properties)

  def apply[P <: WS: WSProtocolFactory](properties: WS.Properties) =
    new IntegratedRoute(properties)


  class IntegratedRoute[P <: WS: WSProtocolFactory](properties: WS.Properties)
      extends WebSocketRoute with Listener[P] {
    private val connected = new ConcurrentLinkedQueue[Connected[P]]

    private def route(authenticatedName: Option[String]) =
      webSocketRoute(this, authenticatedName, properties) { connection =>
        val iterator = connected.iterator
        while (iterator.hasNext)
          iterator.next().fire(connection)
      }

    def apply(authenticatedName: String) = route(Some(authenticatedName))

    def apply(authenticatedName: Option[String]) = route(authenticatedName)

    def apply(v: RequestContext) = route(None)(v)

    protected def startListening(connectionEstablished: Connected[P]): Try[Listening] = {
      connected.add(connectionEstablished)
      Success(new Listening {
        def stopListening(): Unit = connected remove connectionEstablished
      })
    }
  }

  protected sealed abstract class BoundRoute[P <: WS: WSProtocolFactory](
      properties: WS.Properties) extends Listener[P] {
    private var running: Future[Http.ServerBinding] = _
    private val connected = new ConcurrentLinkedQueue[Connected[P]]

    protected def bindRoute(
        http: HttpExt, port: Int, interface: String)(
        connectionEstablished: Try[Connection[P]] => Unit)(implicit
        actorRefFactory: ActorSystem,
        materializer: Materializer) =
      running = http.bindAndHandle(
        webSocketRoute(this, None, properties)(connectionEstablished),
        interface, port)

    protected def connectionEstablished(connection: Try[Connection[P]]) = {
      val iterator = connected.iterator
      while (iterator.hasNext)
        iterator.next().fire(connection)
    }

    protected def startListening(connectionEstablished: Connected[P]): Try[Listening] =
      WSActorSystem synchronized {
        if (connected.isEmpty)
          starting()

        connected.add(connectionEstablished)

        Success(new Listening {
          def stopListening(): Unit = WSActorSystem synchronized {
            connected.remove(connectionEstablished)

            if (connected.isEmpty) {
              stopping()
              running foreach { _.unbind() }
              running = null
            }
          }
        })
      }

    protected def starting(): Unit

    protected def stopping(): Unit
  }

  class DistinctBinding[P <: WS: WSProtocolFactory](
    http: HttpExt, port: Int, interface: String,
    properties: WS.Properties)(implicit
    actorRefFactory: ActorSystem,
    materializer: Materializer)
      extends BoundRoute(properties) {

    protected def starting() =
      bindRoute(http, port, interface)(connectionEstablished)

    protected def stopping() = { }
  }

  class DistinctServer[P <: WS: WSProtocolFactory](
    port: Int, interface: String, properties: WS.Properties)
      extends BoundRoute(properties) {

    protected def starting() = {
      implicit val (actorSystem, actorMaterializer) = WSActorSystem.retrieve()

      bindRoute(Http(), port, interface) { connection =>
        connection foreach { connection =>
          WSActorSystem.retrieve()
          connection.closed foreach { _ => WSActorSystem.release() }
        }
        connectionEstablished(connection)
      }
    }

    protected def stopping() = WSActorSystem.release()
  }
}
