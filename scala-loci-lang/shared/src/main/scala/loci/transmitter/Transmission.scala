package loci
package transmitter

import scala.concurrent.Future

sealed trait Transmission
    [T, R <: Peer, L <: Peer, M <: TieMultiplicity] {
  val id: Any
  def memo[U <: AnyRef](id: Any)(body: => U): U

  val remoteJoined: Notification[Remote[R]]
  val remoteLeft: Notification[Remote[R]]
  def remotes: Seq[Remote[R]]
  def retrieveMappedRemoteValues: Map[Remote[R], Future[T]]
  def retrieveRemoteValues: Seq[Future[T]]
}

sealed trait MultipleTransmission[T, R <: Peer, L <: Peer]
    extends Transmission[T, R, L, MultipleTie] {
  def remotes: Seq[Remote[R]]
  def retrieveMappedRemoteValues: Map[Remote[R], Future[T]]
  def retrieveRemoteValues: Seq[Future[T]] =
    retrieveMappedRemoteValues.values.toSeq
}

sealed trait OptionalTransmission[T, R <: Peer, L <: Peer]
    extends Transmission[T, R, L, OptionalTie] {
  def remote: Option[Remote[R]]
  def retrieveMappedRemoteValue: Option[(Remote[R], Future[T])]
  def retrieveRemoteValue: Option[Future[T]] =
    retrieveMappedRemoteValue map { _._2 }

  def remotes: Seq[Remote[R]] =
    remote.toSeq
  def retrieveMappedRemoteValues: Map[Remote[R], Future[T]] =
    retrieveMappedRemoteValue.toMap
  def retrieveRemoteValues: Seq[Future[T]] =
    retrieveRemoteValue.toSeq

  def multiple: MultipleTransmission[T, R, L]
}

sealed trait SingleTransmission[T, R <: Peer, L <: Peer]
    extends Transmission[T, R, L, SingleTie] {
  def remote: Remote[R]
  def retrieveMappedRemoteValue: (Remote[R], Future[T])
  def retrieveRemoteValue: Future[T] =
    retrieveMappedRemoteValue._2

  def remotes: Seq[Remote[R]] =
    Seq(remote)
  def retrieveMappedRemoteValues: Map[Remote[R], Future[T]] =
    Map(retrieveMappedRemoteValue)
  def retrieveRemoteValues: Seq[Future[T]] =
    Seq(retrieveRemoteValue)

  def optional: OptionalTransmission[T, R, L]
  def multiple: MultipleTransmission[T, R, L]
}


private[loci] trait MultipleTransmissionImplBase[T, R <: Peer, L <: Peer]
  extends MultipleTransmission[T, R, L]

private[loci] trait OptionalTransmissionImplBase[T, R <: Peer, L <: Peer]
  extends OptionalTransmission[T, R, L]

private[loci] trait SingleTransmissionImplBase[T, R <: Peer, L <: Peer]
  extends SingleTransmission[T, R, L]
