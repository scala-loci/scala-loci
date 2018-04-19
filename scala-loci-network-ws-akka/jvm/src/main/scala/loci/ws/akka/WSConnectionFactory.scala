package loci
package ws.akka

import network.ConnectionListener
import network.ConnectionRequestor
import util.Attributes

private object WSConnectionFactory {
  def listener(config: String, attrs: Attributes): Option[ConnectionListener] =
    parse(config) match {
      case Some((_, tls, Some((interface, port)))) =>
        Some(WS(port, interface, secured = tls))
      case _ =>
        None
    }

  def requestor(url: String, attrs: Attributes): Option[ConnectionRequestor] =
    parse(url) map { case (url, _, _) => WS(url) }

  private def parse(str: String): Option[(String, Boolean, Option[(String, Int)])] = {
    val protocolLocation =
      if ((str substring (0, 5) compareToIgnoreCase "ws://") == 0)
        Some((false, str substring 5))
      else if ((str substring (0, 6) compareToIgnoreCase "wss://") == 0)
        Some((true, str substring 6))
      else
        None

    protocolLocation map { case (tls, location) =>
      try location split ":" match {
        case Array(interface, port) =>
          (str, tls, Some((interface, port.toInt)))
        case _ =>
          (str, tls, None)
      }
      catch {
        case _: NumberFormatException =>
          (str, tls, None)
      }
    }
  }
}
