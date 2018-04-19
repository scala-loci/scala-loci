package loci
package impl

import transmission.MultipleTransmissionImplBase
import transmission.OptionalTransmissionImplBase
import transmission.SingleTransmissionImplBase

private final case class MultipleTransmissionImpl[
  T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag](
  system: System, selection: Selection[T, R])
    extends MultipleTransmissionImplBase[T, R, L] {
  val id = (peerTypeOf[R], system, selection)
  def memo[U <: AnyRef](id: Any)(body: => U) = system memo ((this.id, id), body)

  val remoteJoined = system.remotePreJoined[R] transform {
    case remote if selection filter remote => remote
  }
  val remoteLeft = system.remotePreLeft[R] transform {
    case remote if selection filter remote => remote
  }
  def remotes = selection.remotes getOrElse system.preRemotes[R]
  def retrieveMappedRemoteValues =
    (remotes zip system.requestRemotes(selection.props, remotes, true)).toMap
}

private final case class OptionalTransmissionImpl[
  T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag](
  system: System, selection: Selection[T, R])
    extends OptionalTransmissionImplBase[T, R, L] {
  val id = (peerTypeOf[R], system, selection)
  def memo[U <: AnyRef](id: Any)(body: => U) = system memo ((this.id, id), body)

  val remoteJoined = system.remotePreJoined[R] transform {
    case remote if selection filter remote => remote
  }
  val remoteLeft = system.remotePreLeft[R] transform {
    case remote if selection filter remote => remote
  }
  def remote = selection.remote map { Some(_) } getOrElse system.optionalPreRemote[R]
  def retrieveMappedRemoteValue = remote map { remote =>
    remote -> system.requestRemotes(selection.props, Seq(remote), true).head
  }

  def multiple = MultipleTransmissionImpl[T, R, L](system, selection)
}

private final case class SingleTransmissionImpl[
  T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag](
  system: System, selection: Selection[T, R])
    extends SingleTransmissionImplBase[T, R, L] {
  val id = (peerTypeOf[R], system, selection)
  def memo[U <: AnyRef](id: Any)(body: => U) = system memo ((this.id, id), body)

  val remoteJoined = system.remotePreJoined[R] transform {
    case remote if selection filter remote => remote
  }
  val remoteLeft = system.remotePreLeft[R] transform {
    case remote if selection filter remote => remote
  }
  def remote = selection.remote getOrElse system.singlePreRemote[R]
  def retrieveMappedRemoteValue =
    remote -> system.requestRemotes(selection.props, Seq(remote), true).head

  def optional = OptionalTransmissionImpl[T, R, L](system, selection)
  def multiple = MultipleTransmissionImpl[T, R, L](system, selection)
}
