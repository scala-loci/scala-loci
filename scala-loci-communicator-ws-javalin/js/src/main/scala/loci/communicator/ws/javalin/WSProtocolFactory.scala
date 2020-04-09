package loci
package communicator
package ws.javalin

import java.security.cert.Certificate

import scala.util.{Failure, Success, Try}

private sealed trait WSProtocolFactory[P <: WS] {
  def make(url: String, host: Option[String], port: Option[Int],
    setup: ConnectionSetup[P], authenticated: Boolean,
    encrypted: Boolean, integrityProtected: Boolean,
    authentication: Either[Seq[Certificate], String] = Left(Seq.empty)): Try[P]
}

private object WSProtocolFactory {
  locally(WSProtocolFactory)

  implicit object ws extends WSProtocolFactory[WS] {
    def make(url: String, host: Option[String], port: Option[Int],
        setup: ConnectionSetup[WS], authenticated: Boolean,
        encrypted: Boolean, integrityProtected: Boolean,
        authentication: Either[Seq[Certificate], String]): Try[WS] =
      Success(construct(
        url, host, port, setup, authenticated,
        encrypted, integrityProtected, authentication))
  }

  implicit object wsSecure extends WSProtocolFactory[WS.Secure] {
    def make(url: String, host: Option[String], port: Option[Int],
        setup: ConnectionSetup[WS.Secure], authenticated: Boolean,
        encrypted: Boolean, integrityProtected: Boolean,
        authentication: Either[Seq[Certificate], String]): Try[WS.Secure] =
      construct(
          url, host, port, setup, authenticated,
          encrypted, integrityProtected, authentication) match {
        case ws: WS.Secure => Success(ws)
        case _ => Failure(new ConnectionException("connection not secure"))
      }
  }

  private def construct(
      _url: String, _host: Option[String], _port: Option[Int],
      _setup: ConnectionSetup[WS], _authenticated: Boolean,
      _encrypted: Boolean, _integrityProtected: Boolean,
      _authentication: Either[Seq[Certificate], String]) =
    if (_encrypted && _integrityProtected)
      _authentication match {
        case Left(_certificates) if _certificates.isEmpty =>
          new WS.Secure {
            val path = _url;val host = _host;val port  = _port
            val setup = _setup; val authenticated = _authenticated
          }
        case Left(_certificates) =>
          new WS.Secure with CertificateAuthentication {
            val path = _url;val host = _host;val port  = _port
            val setup = _setup; val authenticated = _authenticated
            val certificates = _certificates
          }
        case Right(_name) =>
          new WS.Secure with NameAuthentication {
            val path = _url;val host = _host;val port  = _port
            val setup = _setup; val authenticated = _authenticated
            val name = _name
          }
      }
    else
      _authentication match {
        case Left(_certificates) if _certificates.isEmpty =>
          new WS {
            val path = _url;val host = _host;val port  = _port
            val setup = _setup; val authenticated = _authenticated
            val encrypted = _encrypted
            val integrityProtected = _integrityProtected
          }
        case Left(_certificates) =>
          new WS with CertificateAuthentication {
            val path = _url;val host = _host;val port  = _port
            val setup = _setup; val authenticated = _authenticated
            val encrypted = _encrypted
            val integrityProtected = _integrityProtected
            val certificates = _certificates
          }
        case Right(_name) =>
          new WS with NameAuthentication {
            val path = _url;val host = _host;val port  = _port
            val setup = _setup; val authenticated = _authenticated
            val encrypted = _encrypted
            val integrityProtected = _integrityProtected
            val name = _name
          }
      }
}
