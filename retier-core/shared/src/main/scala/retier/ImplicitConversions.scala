package retier

import typeconstraints._
import scala.language.implicitConversions

protected trait ImplicitConversions {
  final implicit class $$retier$FromExpression[L <: Peer, R <: Peer, T]
      (v: T sharedOn R)
      (implicit
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, R, _]) {
    def from[P <: R]: T from P = `#macro`
    def from[P <: R](peer: Remote[P]): T fromSingle P = `#macro`
    def from[P <: R](peers: Remote[P]*): T fromMultiple P = `#macro`
  }


  final implicit class $$retier$ToExpression[R <: Remote[Peer], T <: (_ localOn _), U]
      (v: T)
      (implicit ev: ValueTypes[T, R, U, _]) {
    def to(peer: R): U = `#macro`
  }


  final implicit def $$retier$value[T <: (_ localOn _), U](v: T)
    (implicit ev: ValueTypes[T, _, _, U]): U = `#macro`


  implicit def $$retier$discardValue[P <: Peer](v: _ localOn P): Unit on P = `#macro`

  implicit def $$retier$issueValue[P <: Peer, R <: Remote[Peer], T](v: T localOn P)
    (implicit ev0: T <:!< (_ <=> _), ev1: T <:!< (_ <-> _)): R <-> T on P = `#macro`

  implicit def $$retier$issueValueControlled[P <: Peer, R <: Remote[Peer], T, U](v: T localOn P)
    (implicit ev: T <:< (R => U)): R <=> U on P = `#macro`


  final implicit def $$retier$liftValueGlobally[P <: Peer, T](v: T)
    (implicit ev0: NoLocalPeer[_], ev1: T <:!< (_ localOn _)): T on P = `#macro`

  final implicit def $$retier$liftValueLocally[P <: Peer, T](v: T)
    (implicit ev0: LocalPeer[P], ev1: T <:!< (_ localOn _)): T on P = `#macro`

  final implicit def $$retier$downcastValueGlobally[P <: Peer, T](v: T localOn P)
    (implicit ev: NoLocalPeer[_]): T on P = `#macro`

  final implicit def $$retier$downcastValueLocally[P <: Peer, T](v: T localOn P)
    (implicit ev: LocalPeer[P]): T on P = `#macro`
}
