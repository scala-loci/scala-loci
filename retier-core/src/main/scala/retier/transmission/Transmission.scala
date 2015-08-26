package retier
package transmission

import scala.concurrent.Future
import util.Notification

sealed trait Transmission
    [T, R <: Peer, L <: Peer, M <: ConnectionMultiplicity] {
  val remoteJoined: Notification[Remote[R]]
  val remoteLeft: Notification[Remote[R]]
  def remotes: Seq[Remote[R]]
  def retrieveMappedRemoteValues: Map[Remote[R], Future[T]]
  def retrieveRemoteValues: Seq[Future[T]]
}

sealed trait MultipleTransmission[T, R <: Peer, L <: Peer]
    extends Transmission[T, R, L, MultipleConnection] {
  def remotes: Seq[Remote[R]]
  def retrieveMappedRemoteValues: Map[Remote[R], Future[T]]
  def retrieveRemoteValues: Seq[Future[T]] =
    retrieveMappedRemoteValues.values.toSeq
}

sealed trait OptionalTransmission[T, R <: Peer, L <: Peer]
    extends Transmission[T, R, L, OptionalConnection] {
  def remote: Option[Remote[R]]
  def retrieveMappedRemoteValue: Option[(Remote[R], Future[T])]
  def retrieveRemoteValue: Option[Future[T]] =
    retrieveMappedRemoteValue map { _._2 }
}

sealed trait SingleTransmission[T, R <: Peer, L <: Peer]
    extends Transmission[T, R, L, SingleConnection] {
  def remote: Remote[R]
  def retrieveMappedRemoteValue: (Remote[R], Future[T])
  def retrieveRemoteValue: Future[T] =
    retrieveMappedRemoteValue._2
}


private trait MultipleTransmissionImplBase[T, R <: Peer, L <: Peer]
  extends MultipleTransmission[T, R, L]

private trait OptionalTransmissionImplBase[T, R <: Peer, L <: Peer]
  extends OptionalTransmission[T, R, L]

private trait SingleTransmissionImplBase[T, R <: Peer, L <: Peer]
  extends SingleTransmission[T, R, L]
