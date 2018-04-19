package loci
package ws.akka

import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.ws.WebSocketRequest
import java.security.cert.Certificate

private case class SecurityProperties(
  isAuthenticated: Boolean, isProtected: Boolean, isEncrypted: Boolean,
  certificates: Seq[Certificate])

private object SecurityProperties {
  final val HTTPS = "https"
  final val WSS = "wss"
  final val NoProtocol = "NONE" // TLSv1, etc.
  final val NoCipher = "SSL_NULL_WITH_NULL_NULL" // see RFC2246, RFC3268, etc.
  final val NoEncryptionFragment = "WITH_NULL"

  def apply(
      responseRequest: Either[(WebSocketRequest, HttpResponse), HttpRequest],
      authenticated: Boolean)
    : SecurityProperties = {

    val (uri, message) = responseRequest match {
      case Left((request, response)) => (request.uri, response)
      case Right(request) => (request.uri, request)
    }

    val tls = uri.scheme == HTTPS || uri.scheme == WSS

    val properties = message.header[`Tls-Session-Info`] map { info =>
      val protocol = info.session.getProtocol
      val cipher = info.session.getCipherSuite

      val tls = protocol != NoProtocol && cipher != NoCipher

      val certificates = info.peerCertificates
      val isAuthenticated = tls && certificates.nonEmpty
      val isProtected = tls
      val isEncrypted = tls && !(cipher contains NoEncryptionFragment)

      SecurityProperties(
        authenticated || isAuthenticated, isProtected, isEncrypted, certificates)
    }

    properties getOrElse {
      SecurityProperties(authenticated, tls, tls, Seq.empty)
    }
  }
}
