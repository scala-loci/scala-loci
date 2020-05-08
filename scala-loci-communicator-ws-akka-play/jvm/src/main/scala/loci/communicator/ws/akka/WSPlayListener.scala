package loci
package communicator
package ws.akka

import java.net.URI
import java.util.concurrent.ConcurrentLinkedQueue

import play.api.mvc.Security.AuthenticatedRequest
import play.api.mvc.{RequestHeader, Results, WebSocket}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

private object WSPlayListener {
  locally(WSPlayListener)

  def apply[P <: WS: WSProtocolFactory](properties: WS.Properties) =
    new Listener[P] with WebSocketHandler {
      private def webSocket(authenticated: Either[Option[String], Any]) =
        WebSocket { request =>
          val uri = new URI(s"dummy://${request.host}")
          val host = uri.getHost
          val port = uri.getPort

          val certificates = request.clientCertificateChain.toSeq.flatten
          val isAuthenticated =
            authenticated.isRight ||
            compatibility.either.left(authenticated).nonEmpty ||
            (request.secure && certificates.nonEmpty)
          val isProtected = request.secure
          val isEncrypted = request.secure

          val ws = implicitly[WSProtocolFactory[P]] make (
            request.uri,
            Option(host),
            if (port < 0) None else Some(port),
            this, isAuthenticated, isEncrypted, isProtected,
            Some(Left(request)),
            authenticated.left.toOption.flatten toRight certificates)

          Future successful (ws match {
            case Failure(exception) =>
              connectionEstablished(Failure(exception))
              Left(Results.NotFound)

            case Success(ws) =>
              Right(WSPlayHandler handleWebSocket (
                Future successful ws, properties, connectionEstablished))
          })
        }

      def apply(authenticatedName: String) = webSocket(Left(Some(authenticatedName)))
      def apply(authenticatedName: Option[String]) = webSocket(Left(authenticatedName))
      def apply(request: RequestHeader) = request match {
        case request: AuthenticatedRequest[_, _] =>
          request.user match {
            case user: String =>
              webSocket(Left(Some(user)))(request)
            case user =>
              webSocket(Right(user))(request)
          }
        case _ =>
          webSocket(Left(None))(request)
      }

      private val connected = new ConcurrentLinkedQueue[Connected[P]]

      private def connectionEstablished(connection: Try[Connection[P]]) = {
        val iterator = connected.iterator
        while (iterator.hasNext)
          iterator.next().fire(connection)
      }

      protected def startListening(connectionEstablished: Connected[P]): Try[Listening] = {
        connected.add(connectionEstablished)

        Success(new Listening {
          def stopListening(): Unit = connected.remove(connectionEstablished)
        })
      }
    }
}
