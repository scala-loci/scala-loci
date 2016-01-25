package retier
package ws.akka

import network.Connection
import network.ConnectionRequestor
import contexts.Immediate.Implicits.global
import akka.stream.Materializer
import akka.stream.ConnectionException
import akka.http.scaladsl.Http
import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.ws.WebsocketRequest
import akka.http.scaladsl.model.ws.ValidUpgrade
import akka.http.scaladsl.model.ws.InvalidUpgradeResponse
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.control.NonFatal
import scala.util.Failure
import scala.util.Success

private object WSConnectionRequestor {
  def apply(
      http: HttpExt, websocketRequest: WebsocketRequest)(implicit
      materializer: Materializer) =
    new Connector(
      http, websocketRequest, Function const { }, Function const { })

  def apply(websocketRequest: WebsocketRequest) = {
    implicit val (actorSystem, actorMaterializer) = WSActorSystem.retrieve
    new Connector(
      Http(), websocketRequest,
      { _.closed += { _ => WSActorSystem.release } },
      { _ => WSActorSystem.release })
  }

  class Connector(
    http: HttpExt,
    websocketRequest: WebsocketRequest,
    websocketConnectionEstablished: Connection => Unit,
    websocketConnectionFailed: Throwable => Unit)(implicit
    materializer: Materializer)
      extends ConnectionRequestor {

    def request = {
      val promise = Promise[Connection]
      val websocketPromise = Promise[Connection]
      val protocolPromise = Promise[WS]

      def connectionEstablished(connection: Connection) = {
        websocketConnectionEstablished(connection)
        promise.future onFailure PartialFunction { _ => connection.close }
        websocketPromise success connection
      }

      def connectionFailed(cause: Throwable) = {
        if (!websocketPromise.isCompleted)
          websocketConnectionFailed(cause)
        promise tryFailure cause
      }

      val (future, _) =
        try http singleWebsocketRequest (
          websocketRequest,
          WSConnectionHandler handleWebsocket (
            protocolPromise.future, connectionEstablished, connectionFailed))
        catch {
          case NonFatal(exception) => (Future failed exception, ())
        }

      future onComplete {
        case Success(ValidUpgrade(_, _)) =>
          val uri = websocketRequest.uri
          val host = uri.authority.host.address
          val port = uri.effectivePort
          val tls = websocketRequest.uri.scheme == "wss"

          protocolPromise success
            WS.createProtocolInfo(
              uri.toString, Some(host), Some(port), tls, tls, tls)

          promise tryCompleteWith websocketPromise.future

        case Success(InvalidUpgradeResponse(_, cause)) =>
          connectionFailed(new ConnectionException(cause))

        case Failure(exception) =>
          connectionFailed(exception)
      }

      promise.future
    }
  }
}
