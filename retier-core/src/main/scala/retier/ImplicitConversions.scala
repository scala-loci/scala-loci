package retier

import typeconstraints._
import scala.language.implicitConversions

protected trait ImplicitConversions {
  final implicit class $$retier$ValueOp[T <: (_ localOn _), U](v: T)
      (implicit ev: ValueTypes[T, U]) {
    def value: U = `#macro`
  }

  final implicit def $$retier$value[T <: (_ localOn _), U](v: T)
    (implicit ev: ValueTypes[T, U]): U = `#macro`


  implicit def $$retier$discardValue[P <: Peer](v: _ localOn P): Unit on P = `#macro`

  implicit def $$retier$issueValue[P <: Peer, R <: Remote[Peer], T](v: T localOn P)
    (implicit ev: T <:!< (_ <=> _)): R <-> T on P = `#macro`

  implicit def $$retier$issueValueControlled[P <: Peer, R <: Remote[Peer], T, U](v: T localOn P)
    (implicit ev0: T <:!< (_ <=> _), ev1: T <:< (R => U)): R <=> U on P = `#macro`


  final implicit def $$retier$liftValueGlobally[P <: Peer, T](v: T)
    (implicit ev0: NoLocalPeer[_], ev1: T <:!< (_ localOn _)): T on P = `#macro`

  final implicit def $$retier$liftValueLocally[P <: Peer, T](v: T)
    (implicit ev0: LocalPeer[P], ev1: T <:!< (_ localOn _)): T on P = `#macro`

  final implicit def $$retier$downcastValueGlobally[P <: Peer, T](v: T localOn P)
    (implicit ev: NoLocalPeer[_]): T on P = `#macro`

  final implicit def $$retier$downcastValueLocally[P <: Peer, T](v: T localOn P)
    (implicit ev: LocalPeer[P]): T on P = `#macro`
}
