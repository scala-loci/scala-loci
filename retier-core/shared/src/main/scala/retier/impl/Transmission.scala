package retier
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

  val remoteJoined = system.remoteJoined[R] transform {
    case remote if selection filter remote => remote
  }
  val remoteLeft = system.remoteLeft[R] transform {
    case remote if selection filter remote => remote
  }
  def remotes = system.remotes[R] filter selection.filter
  def retrieveMappedRemoteValues =
    (remotes zip system.requestRemotes(selection.props, remotes)).toMap
}

private final case class OptionalTransmissionImpl[
  T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag](
  system: System, selection: Selection[T, R])
    extends OptionalTransmissionImplBase[T, R, L] {
  val id = (peerTypeOf[R], system, selection)
  def memo[U <: AnyRef](id: Any)(body: => U) = system memo ((this.id, id), body)

  val remoteJoined = system.remoteJoined[R] transform {
    case remote if selection filter remote => remote
  }
  val remoteLeft = system.remoteLeft[R] transform {
    case remote if selection filter remote => remote
  }
  def remote = (system.remotes[R] filter selection.filter).headOption
  def retrieveMappedRemoteValue = remote map { remote =>
    remote -> system.requestRemotes(selection.props, Seq(remote)).head
  }
}

private final case class SingleTransmissionImpl[
  T, R <: Peer: PeerTypeTag, L <: Peer: PeerTypeTag](
  system: System, props: TransmissionProperties[T])
    extends SingleTransmissionImplBase[T, R, L] {
  val id = (peerTypeOf[R], system, props)
  def memo[U <: AnyRef](id: Any)(body: => U) = system memo ((this.id, id), body)

  val remoteJoined = system.remoteJoined[R]
  val remoteLeft = system.remoteLeft[R]
  def remote = system.singleRemote[R]
  def retrieveMappedRemoteValue =
    remote -> system.requestRemotes(props, Seq(remote)).head
}
