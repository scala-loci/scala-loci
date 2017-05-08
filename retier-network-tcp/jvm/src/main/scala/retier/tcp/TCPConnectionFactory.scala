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
    if ((str substring (0, 6) compareToIgnoreCase "tcp://") == 0) {
      val index = str lastIndexOf ':'
      try
        if (index != 3) {
          val hostInterface = str substring (6, index)
          val port = Some((str substring (index + 1)).toInt)
          if (hostInterface.nonEmpty &&
              hostInterface.head == '[' && hostInterface.last == ']')
            (Some(hostInterface substring (1, hostInterface.length - 1)), port)
          else
            (Some(hostInterface), port)
        }
        else
          (None, Some((str substring 6).toInt))
      catch {
        case _: NumberFormatException =>
          (None, None)
      }
    }
    else
      (None, None)
  }
}
