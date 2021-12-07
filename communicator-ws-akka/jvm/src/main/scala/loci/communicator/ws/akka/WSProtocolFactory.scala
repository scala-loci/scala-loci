package loci
package communicator
package ws.akka

import java.security.cert.Certificate

import scala.util.{Failure, Success, Try}

private sealed trait WSProtocolFactory[P <: WS] {
  def make(url: String, host: Option[String], port: Option[Int],
    setup: ConnectionSetup[P], authenticated: Boolean,
    encrypted: Boolean, integrityProtected: Boolean,
    requestResponse: Option[Either[Any, Any]] = None,
    authentication: Either[Seq[Certificate], String] = Left(Seq.empty)): Try[P]
}

private object WSProtocolFactory {
  locally(WSProtocolFactory)

  implicit object ws extends WSProtocolFactory[WS] {
    def make(url: String, host: Option[String], port: Option[Int],
        setup: ConnectionSetup[WS], authenticated: Boolean,
        encrypted: Boolean, integrityProtected: Boolean,
        requestResponse: Option[Either[Any, Any]],
        authentication: Either[Seq[Certificate], String]): Try[WS] =
      Success(construct(
        url, host, port, setup, authenticated,
        encrypted, integrityProtected,
        requestResponse, authentication))
  }

  implicit object wsSecure extends WSProtocolFactory[WS.Secure] {
    def make(url: String, host: Option[String], port: Option[Int],
        setup: ConnectionSetup[WS.Secure], authenticated: Boolean,
        encrypted: Boolean, integrityProtected: Boolean,
        requestResponse: Option[Either[Any, Any]],
        authentication: Either[Seq[Certificate], String]): Try[WS.Secure] =
      construct(
          url, host, port, setup, authenticated,
          encrypted, integrityProtected,
          requestResponse, authentication) match {
        case ws: WS.Secure => Success(ws)
        case _ => Failure(new ConnectionException("connection not secure"))
      }
  }

  private def construct(
      _url: String, _host: Option[String], _port: Option[Int],
      _setup: ConnectionSetup[WS], _authenticated: Boolean,
      _encrypted: Boolean, _integrityProtected: Boolean,
      _requestResponse: Option[Either[Any, Any]],
      _authentication: Either[Seq[Certificate], String]) =
    _requestResponse match {
      case Some(Left(_request)) =>
        if (_encrypted && _integrityProtected)
          _authentication match {
            case Left(_certificates) if _certificates.isEmpty =>
              new WS.Secure with RequestInfo {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val request = _request
              }
            case Left(_certificates) =>
              new WS.Secure with RequestInfo with CertificateAuthentication {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val request = _request; val certificates = _certificates
              }
            case Right(_name) =>
              new WS.Secure with RequestInfo with NameAuthentication {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val request = _request; val name = _name
              }
          }
        else
          _authentication match {
            case Left(_certificates) if _certificates.isEmpty =>
              new WS with RequestInfo {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val encrypted = _encrypted
                val integrityProtected = _integrityProtected
                val request = _request
              }
            case Left(_certificates) =>
              new WS with RequestInfo with CertificateAuthentication {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val encrypted = _encrypted
                val integrityProtected = _integrityProtected
                val request = _request; val certificates = _certificates
              }
            case Right(_name) =>
              new WS with RequestInfo with NameAuthentication {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val encrypted = _encrypted
                val integrityProtected = _integrityProtected
                val request = _request; val name = _name
              }
          }

      case Some(Right(_response)) =>
        if (_encrypted && _integrityProtected)
          _authentication match {
            case Left(_certificates) if _certificates.isEmpty =>
              new WS.Secure with ResponseInfo {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val response = _response
              }
            case Left(_certificates) =>
              new WS.Secure with ResponseInfo with CertificateAuthentication {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val response = _response; val certificates = _certificates
              }
            case Right(_name) =>
              new WS.Secure with ResponseInfo with NameAuthentication {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val response = _response; val name = _name
              }
          }
        else
          _authentication match {
            case Left(_certificates) if _certificates.isEmpty =>
              new WS with ResponseInfo {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val encrypted = _encrypted
                val integrityProtected = _integrityProtected
                val response = _response
              }
            case Left(_certificates) =>
              new WS with ResponseInfo with CertificateAuthentication {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val encrypted = _encrypted
                val integrityProtected = _integrityProtected
                val response = _response; val certificates = _certificates
              }
            case Right(_name) =>
              new WS with ResponseInfo with NameAuthentication {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val encrypted = _encrypted
                val integrityProtected = _integrityProtected
                val response = _response; val name = _name
              }
          }

      case _ =>
        if (_encrypted && _integrityProtected)
          _authentication match {
            case Left(_certificates) if _certificates.isEmpty =>
              new WS.Secure {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
              }
            case Left(_certificates) =>
              new WS.Secure with CertificateAuthentication {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val certificates = _certificates
              }
            case Right(_name) =>
              new WS.Secure with NameAuthentication {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val name = _name
              }
          }
        else
          _authentication match {
            case Left(_certificates) if _certificates.isEmpty =>
              new WS {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val encrypted = _encrypted
                val integrityProtected = _integrityProtected
              }
            case Left(_certificates) =>
              new WS with CertificateAuthentication {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val encrypted = _encrypted
                val integrityProtected = _integrityProtected
                val certificates = _certificates
              }
            case Right(_name) =>
              new WS with NameAuthentication {
                val url = _url; val host = _host; val port = _port
                val setup = _setup; val authenticated = _authenticated
                val encrypted = _encrypted
                val integrityProtected = _integrityProtected
                val name = _name
              }
          }
    }
}
