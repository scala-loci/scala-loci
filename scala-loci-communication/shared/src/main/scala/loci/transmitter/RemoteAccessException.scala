package loci
package transmitter

class RemoteAccessException(val reason: RemoteAccessException.Reason) extends RuntimeException(reason.report) {
  def this(message: String) = this(RemoteAccessException.Violation(message))
}

object RemoteAccessException {
  sealed abstract class Reason(val report: String)
  case class Violation(message: String) extends Reason(message)
  case object RemoteDisconnected extends Reason("remote disconnected")
  case object ChannelClosed extends Reason("communication channel closed")
}
