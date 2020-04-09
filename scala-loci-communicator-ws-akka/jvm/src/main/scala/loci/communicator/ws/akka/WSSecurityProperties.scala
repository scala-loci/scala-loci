package loci
package communicator
package ws.akka

import java.security.cert.Certificate

import akka.http.scaladsl.model.{HttpMessage, HttpRequest, HttpResponse, Uri}
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.ws.WebSocketRequest

private case class WSSecurityProperties(
  isAuthenticated: Boolean, isProtected: Boolean, isEncrypted: Boolean,
  certificates: Seq[Certificate])

private object WSSecurityProperties {
  final val HTTPS = "https"
  final val WSS = "wss"
  final val NoProtocol = "NONE" // TLSv1, etc.
  final val NoCipher = "SSL_NULL_WITH_NULL_NULL" // see RFC2246, RFC3268, etc.
  final val NoEncryptionFragment = "WITH_NULL"

  def apply(request: WebSocketRequest, response: HttpResponse,
      authenticated: Boolean): WSSecurityProperties =
    create(request.uri, response, authenticated)

  def apply(request: HttpRequest, authenticated: Boolean): WSSecurityProperties =
    create(request.uri, request, authenticated)

  private def create(uri: Uri, message: HttpMessage, authenticated: Boolean)
    : WSSecurityProperties = {

    val tls = uri.scheme == HTTPS || uri.scheme == WSS

    val properties = message.header[`Tls-Session-Info`] map { info =>
      val protocol = info.session.getProtocol
      val cipher = info.session.getCipherSuite

      val tls = protocol != NoProtocol && cipher != NoCipher

      val certificates = info.peerCertificates
      val isAuthenticated = tls && certificates.nonEmpty
      val isProtected = tls
      val isEncrypted = tls && !(cipher contains NoEncryptionFragment)

      WSSecurityProperties(authenticated || isAuthenticated, isProtected, isEncrypted, certificates)
    }

    properties getOrElse { WSSecurityProperties(authenticated, tls, tls, Seq.empty) }
  }
}
