package retier
package tcp

import network.ConnectionListener
import network.ConnectionRequestor
import util.Attributes

private object TCPConnectionFactory {
  def listener(config: String, attrs: Attributes): Option[ConnectionListener] =
    parse(config) match {
      case (Some(interface), Some(port)) => Some(TCP(port, interface))
      case (None, Some(port)) => Some(TCP(port))
      case _ => None
    }

  def requestor(url: String, attrs: Attributes): Option[ConnectionRequestor] =
    parse(url) match {
      case (Some(host), Some(port)) => Some(TCP(host, port))
      case _ => None
    }

  private def parse(str: String): (Option[String], Option[Int]) = {
    if ((str substring (0, 6) compareToIgnoreCase "tcp://") == 0)
      try str substring 6 split ":" match {
        case Array(hostInterface, port) =>
          (Some(hostInterface), Some(port.toInt))
        case Array(port) =>
          (None, Some(port.toInt))
        case _ =>
          (None, None)
      }
      catch {
        case _: NumberFormatException =>
          (None, None)
      }
    else
      (None, None)
  }
}
