package loci
package communicator
package ws.jetty

import org.eclipse.jetty.websocket.servlet.ServletUpgradeRequest

import scala.util.{Failure, Success, Try}

private sealed trait WSProtocolFactory[P <: WS] {
  def make(url: String, host: Option[String], port: Option[Int],
    setup: ConnectionSetup[P], authenticated: Boolean,
    encrypted: Boolean, integrityProtected: Boolean,
    request: Option[ServletUpgradeRequest]): Try[P]
}

private object WSProtocolFactory {
  locally(WSProtocolFactory)

  implicit object ws extends WSProtocolFactory[WS] {
    def make(url: String, host: Option[String], port: Option[Int],
        setup: ConnectionSetup[WS], authenticated: Boolean,
        encrypted: Boolean, integrityProtected: Boolean,
        request: Option[ServletUpgradeRequest]): Try[WS] =
      Success(construct(
        url, host, port, setup, authenticated,
        encrypted, integrityProtected, request))
  }

  implicit object wsSecure extends WSProtocolFactory[WS.Secure] {
    def make(url: String, host: Option[String], port: Option[Int],
        setup: ConnectionSetup[WS.Secure], authenticated: Boolean,
        encrypted: Boolean, integrityProtected: Boolean,
        request: Option[ServletUpgradeRequest]): Try[WS.Secure] =
      construct(
          url, host, port, setup, authenticated,
          encrypted, integrityProtected, request) match {
        case ws: WS.Secure => Success(ws)
        case _ => Failure(new ConnectionException("connection not secure"))
      }
  }

  private def construct(
      _url: String, _host: Option[String], _port: Option[Int],
      _setup: ConnectionSetup[WS], _authenticated: Boolean,
      _encrypted: Boolean, _integrityProtected: Boolean,
      _request: Option[ServletUpgradeRequest]) =
    if (_encrypted && _integrityProtected)
      new WS.Secure {
        val path = _url;val host = _host;val port  = _port
        val setup = _setup; val authenticated = _authenticated
        val request = _request
      }
    else
      new WS {
        val path = _url;val host = _host;val port  = _port
        val setup = _setup; val authenticated = _authenticated
        val encrypted = _encrypted
        val integrityProtected = _integrityProtected
        val request = _request
      }
}
