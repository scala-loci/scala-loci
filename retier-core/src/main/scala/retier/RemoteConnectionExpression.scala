package retier

import scala.language.implicitConversions

protected final abstract class RemoteConnectionExpression[P <: Peer]

object RemoteConnectionExpression {
  implicit def multipleConnection
    [R <: Peer, L <: Peer, Provider <: RemoteConnectionInterface]
    (v: RemoteConnectionExpression[R])
    (implicit
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, R, MultipleConnection],
        ev2: MultipleRemoteConnection[R] => Provider): Provider = `#macro`

  implicit def optionalConnection
    [R <: Peer, L <: Peer, Provider <: RemoteConnectionInterface]
    (v: RemoteConnectionExpression[R])
    (implicit
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, R, OptionalConnection],
        ev2: OptionalRemoteConnection[R] => Provider): Provider = `#macro`

  implicit def singleConnection
    [R <: Peer, L <: Peer, Provider <: RemoteConnectionInterface]
    (v: RemoteConnectionExpression[R])
    (implicit
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, R, SingleConnection],
        ev2: SingleRemoteConnection[R] => Provider): Provider = `#macro`
}
