package loci
package communicator
package ws.akka

import akka.stream.Materializer
import akka.stream.ConnectionException
import akka.http.scaladsl.Http
import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.ws.WebSocketRequest
import akka.http.scaladsl.model.ws.ValidUpgrade
import akka.http.scaladsl.model.ws.InvalidUpgradeResponse
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal
import scala.util.Try
import scala.util.Failure
import scala.util.Success

private object WSConnector {
  locally(WSConnector)

  def apply[P <: WS: WSProtocolFactory](
      http: HttpExt,
      webSocketRequest: WebSocketRequest,
      properties: WS.Properties)(implicit
      materializer: Materializer) =
    new WSConnector[P](
      http, properties, webSocketRequest, Function const { })

  def apply[P <: WS: WSProtocolFactory](
      webSocketRequest: WebSocketRequest,
      properties: WS.Properties) = {
    implicit val (actorSystem, actorMaterializer) = WSActorSystem.retrieve
    new WSConnector[P](
      Http(), properties, webSocketRequest, {
        case Success(connection) =>
          connection.closed notify { _ => WSActorSystem.release }
        case _ =>
          WSActorSystem.release
      })
  }

  class WSConnector[P <: WS: WSProtocolFactory](
    http: HttpExt,
    properties: WS.Properties,
    webSocketRequest: WebSocketRequest,
    webSocketConnectionEstablished: Try[Connection[P]] => Unit)(implicit
    materializer: Materializer)
      extends Connector[P] {

    def connect(handler: Handler[P]) = {
      val protocolPromise = Promise[P]

      def connectionEstablished(connection: Try[Connection[P]]) = {
        webSocketConnectionEstablished(connection)
        handler notify connection
      }

      val (future, _) =
        try http singleWebSocketRequest (
          webSocketRequest,
          WSHandler handleWebSocket (
            protocolPromise.future, properties, connectionEstablished))
        catch {
          case NonFatal(exception) => (Future failed exception, ())
        }

      future onComplete {
        case Success(ValidUpgrade(response, _)) =>
          val uri = webSocketRequest.uri

          val SecurityProperties(
              isAuthenticated, isProtected, isEncrypted, certificates) =
            SecurityProperties(
              webSocketRequest, response, authenticated = false)

          implicitly[WSProtocolFactory[P]] make (
              uri.toString,
              Some(uri.authority.host.address), Some(uri.effectivePort),
              WSConnector.this, isAuthenticated, isEncrypted, isProtected,
              Some(Right(response)), Left(certificates))  match {
            case Failure(exception) =>
              connectionEstablished(Failure(exception))

            case Success(ws) =>
              protocolPromise success ws
          }

        case Success(InvalidUpgradeResponse(_, cause)) =>
          connectionEstablished(Failure(new ConnectionException(cause)))

        case Failure(exception) =>
          connectionEstablished(Failure(exception))
      }
    }
  }
}
