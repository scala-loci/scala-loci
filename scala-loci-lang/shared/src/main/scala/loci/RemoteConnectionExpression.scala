package loci

import scala.language.implicitConversions

protected final abstract class RemoteConnectionExpression[P <: Peer]

object RemoteConnectionExpression extends
    ide.intellij.RemoteConnectionConversions {

  implicit def multipleConnection
    [R <: Peer, L <: Peer, Provider <: RemoteConnectionInterface]
    (v: RemoteConnectionExpression[R])
    (implicit
        ev0: LocalPeer[L],
        ev1: PeerTie[L#Tie, R, MultipleTie],
        ev2: MultipleRemoteConnection[R] => Provider): Provider = `#macro`(ev0, ev1, ev2)

  implicit def optionalConnection
    [R <: Peer, L <: Peer, Provider <: RemoteConnectionInterface]
    (v: RemoteConnectionExpression[R])
    (implicit
        ev0: LocalPeer[L],
        ev1: PeerTie[L#Tie, R, OptionalTie],
        ev2: OptionalRemoteConnection[R] => Provider): Provider = `#macro`(ev0, ev1, ev2)

  implicit def singleConnection
    [R <: Peer, L <: Peer, Provider <: RemoteConnectionInterface]
    (v: RemoteConnectionExpression[R])
    (implicit
        ev0: LocalPeer[L],
        ev1: PeerTie[L#Tie, R, SingleTie],
        ev2: SingleRemoteConnection[R] => Provider): Provider = `#macro`(ev0, ev1, ev2)
}
