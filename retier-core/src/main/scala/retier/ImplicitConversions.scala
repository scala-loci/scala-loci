package retier

import typeconstraints._
import scala.language.implicitConversions

protected trait ImplicitConversions {
  final implicit class ValueOp[T <: ValueProxy, U](v: T)
      (implicit ev: ValueTypes[T, U]) {
    def value: U = `#macro`
  }

  final implicit def value[T <: ValueProxy, U](v: T)
    (implicit ev: ValueTypes[T, U]): U = `#macro`


  implicit def discardValue[P <: Peer](v: _ localOn P): Unit on P = `#macro`

  implicit def issueValue[P <: Peer, R <: Remote[Peer], T](v: T localOn P)
    (implicit ev: T <:!< (_ <=> _)): R <-> T on P = `#macro`

  implicit def issueValueControlled[P <: Peer, R <: Remote[Peer], T, U](v: T localOn P)
    (implicit ev0: T <:!< (_ <=> _), ev1: T <:< (R => U)): R <=> U on P = `#macro`


  final implicit def reduceCapture[T, U](v: Captured[T])
    (implicit ev: T <:< (U localOn _)): Captured[U] = `#macro`

  final implicit def liftCapture[P <: Peer, T, U](v: Captured[T])
    (implicit ev0: LocalPeer[P], ev1: ValueTypes[Captured[T], U]): U on P = `#macro`


  final implicit def liftValueGlobally[P <: Peer, T](v: T)
    (implicit ev0: NoLocalPeer[_], ev1: T <:!< (_ localOn _)): T on P = `#macro`

  final implicit def liftValueLocally[P <: Peer, T](v: T)
    (implicit ev0: LocalPeer[P], ev1: T <:!< (_ localOn _)): T on P = `#macro`

  final implicit def downcastValueGlobally[P <: Peer, T](v: T localOn P)
    (implicit ev: NoLocalPeer[_]): T on P = `#macro`

  final implicit def downcastValueLocally[P <: Peer, T](v: T localOn P)
    (implicit ev: LocalPeer[P]): T on P = `#macro`
}
