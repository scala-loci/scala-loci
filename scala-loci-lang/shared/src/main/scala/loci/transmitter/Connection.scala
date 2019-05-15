package loci
package transmitter

import loci.communicator._
import loci.language._
import loci.messaging._

abstract class Connection[R, M] private[loci] {
  private[loci] def cache[B <: AnyRef](id: Any, body: => B): B
  private[loci] val remoteJoined: Notification[Remote[R]]
  private[loci] val remoteLeft: Notification[Remote[R]]
  private[loci] def remoteReferences: Seq[Remote[R]]
  private[loci] def remoteConnect(connector: Connector[ConnectionsBase.Protocol]): Unit
}

object Connection {
  implicit def connection[L, R, N, M](implicit
    ev0: Placement.Context[L],
    ev1: Tie[L, R, N],
    ev2: M =:= N): Connection[R, M] = erased(ev0, ev1, ev2)
}
