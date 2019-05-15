package loci.dev
package transmitter

import loci.communicator._
import loci.dev.language._

abstract class Connection[R, M] private[dev] {
  private[dev] val remoteJoined: loci.Notification[Remote[R]]
  private[dev] val remoteLeft: loci.Notification[Remote[R]]
  private[dev] def multipleRemotes: Seq[Remote[R]]
  private[dev] def optionalRemote: Option[Remote[R]]
  private[dev] def singleRemote: Remote[R]
  private[dev] def connectRemote(connector: Connector[ProtocolCommon]): Unit
}

object Connection {
  implicit def connection[L, R, N, M](implicit
    ev0: Placement.Context[L],
    ev1: Tie[L, R, N],
    ev2: M =:= N): Connection[R, M] = erased(ev0, ev1, ev2)
}
