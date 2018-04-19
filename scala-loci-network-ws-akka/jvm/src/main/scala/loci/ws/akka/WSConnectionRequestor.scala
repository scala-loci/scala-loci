package loci
package ws.akka

import network.Connection
import network.ConnectionRequestor
import contexts.Immediate.Implicits.global
import akka.stream.Materializer
import akka.stream.ConnectionException
import akka.http.scaladsl.Http
import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.ws.WebSocketRequest
import akka.http.scaladsl.model.ws.ValidUpgrade
import akka.http.scaladsl.model.ws.InvalidUpgradeResponse
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.control.NonFatal
import scala.util.Failure
import scala.util.Success

private object WSConnectionRequestor {
  def apply(
      http: HttpExt, webSocketRequest: WebSocketRequest)(implicit
      materializer: Materializer) =
    new Connector(
      http, webSocketRequest, Function const { }, Function const { })

  def apply(webSocketRequest: WebSocketRequest) = {
    implicit val (actorSystem, actorMaterializer) = WSActorSystem.retrieve
    new Connector(
      Http(), webSocketRequest,
      { _.closed += { _ => WSActorSystem.release } },
      { _ => WSActorSystem.release })
  }

  class Connector(
    http: HttpExt,
    webSocketRequest: WebSocketRequest,
    webSocketConnectionEstablished: Connection => Unit,
    webSocketConnectionFailed: Throwable => Unit)(implicit
    materializer: Materializer)
      extends ConnectionRequestor {

    def request = {
      val promise = Promise[Connection]
      val webSocketPromise = Promise[Connection]
      val protocolPromise = Promise[WS]

      def connectionEstablished(connection: Connection) = {
        webSocketConnectionEstablished(connection)
        promise.future.failed foreach { _ => connection.close }
        webSocketPromise success connection
      }

      def connectionFailed(cause: Throwable) = {
        if (!webSocketPromise.isCompleted)
          webSocketConnectionFailed(cause)
        promise tryFailure cause
      }

      val (future, _) =
        try http singleWebSocketRequest (
          webSocketRequest,
          WSConnectionHandler handleWebSocket (
            protocolPromise.future, connectionEstablished, connectionFailed))
        catch {
          case NonFatal(exception) => (Future failed exception, ())
        }

      future onComplete {
        case Success(ValidUpgrade(response, _)) =>
          val uri = webSocketRequest.uri
          val host = uri.authority.host.address
          val port = uri.effectivePort

          val SecurityProperties(
              isAuthenticated, isProtected, isEncrypted, certificates) =
            SecurityProperties(
              Left((webSocketRequest, response)),
              authenticated = false)

          protocolPromise success
            WS.createProtocolInfo(
              uri.toString, Some(host), Some(port),
              this, isEncrypted, isProtected, isAuthenticated,
              Some((response, certificates)))

          promise tryCompleteWith webSocketPromise.future

        case Success(InvalidUpgradeResponse(_, cause)) =>
          connectionFailed(new ConnectionException(cause))

        case Failure(exception) =>
          connectionFailed(exception)
      }

      promise.future
    }
  }
}
