package retier

import typeconstraints._
import scala.language.implicitConversions

protected trait ImplicitConversions {
  private final abstract class ValueTypes[T, U]

  private object ValueTypes {
    implicit def placedValueNonIssued[P <: Peer, T, U]
      (implicit
          ev0: T <:< (U `local on` P),
          ev1: LocalPeer[P],
          ev2: U <:!< (_ <=> _)): ValueTypes[T, U] = `#macro`
    implicit def placedValueControlledIssued[P <: Peer, R <: Remote[Peer], T, U, V]
      (implicit
          ev0: T <:< (U `local on` P),
          ev1: LocalPeer[P],
          ev2: U <:< (R <=> V), ev3: U <:!< (_ <-> _)): ValueTypes[T, R => V] = `#macro`
    implicit def placedValueIssued[P <: Peer, T, U, V]
      (implicit
          ev0: T <:< (U `local on` P),
          ev1: LocalPeer[P],
          ev2: U <:< (_ <-> V)): ValueTypes[T, V] = `#macro`
    implicit def capturedValueCaptured[T, U, V]
      (implicit
          ev0: T <:< Captured[U],
          ev1: U <:< Captured[_],          
          ev2: ValueTypes[U, V]): ValueTypes[T, V] = `#macro`
    implicit def capturedValueNonPlaced[T, U]
      (implicit
          ev0: T <:< Captured[U],
          ev1: U <:!< Captured[_],
          ev2: U <:!< (_ `local on` _)): ValueTypes[T, U] = `#macro`
    implicit def capturedValuePlaced[T, U, V]
      (implicit
          ev0: T <:< Captured[U],
          ev1: U <:< (V `local on` _)): ValueTypes[T, V] = `#macro`
  }

  final implicit class ValueOp[T <: ValueProxy, U](v: T)
      (implicit ev: ValueTypes[T, U]) {
    def value: U = `#macro`
  }

  final implicit def value[T <: ValueProxy, U](v: T)
    (implicit ev: ValueTypes[T, U]): U = `#macro`


  implicit def discardValue[P <: Peer](v: _ `local on` P): Unit on P = `#macro`

  implicit def issueValue[P <: Peer, R <: Remote[Peer], T](v: T `local on` P)
    (implicit ev: T <:!< (_ <=> _)): R <-> T on P = `#macro`

  implicit def issueValueControlled[P <: Peer, R <: Remote[Peer], T, U](v: T `local on` P)
    (implicit ev0: T <:!< (_ <=> _), ev1: T <:< (R => U)): R <=> U on P = `#macro`


  final implicit def reduceCapture[T, U](v: Captured[T])
    (implicit ev: T <:< (U `local on` _)): Captured[U] = `#macro`

  final implicit def liftCapture[P <: Peer, T, U](v: Captured[T])
    (implicit ev0: LocalPeer[P], ev1: ValueTypes[Captured[T], U]): U on P = `#macro`


  final implicit def liftValueGlobally[P <: Peer, T](v: T)
    (implicit ev0: NoLocalPeer[_], ev1: T <:!< (_ `local on` _)): T on P = `#macro`

  final implicit def liftValueLocally[P <: Peer, T](v: T)
    (implicit ev0: LocalPeer[P], ev1: T <:!< (_ `local on` _)): T on P = `#macro`

  final implicit def downcastValueGlobally[P <: Peer, T](v: T `local on` P)
    (implicit ev: NoLocalPeer[_]): T on P = `#macro`

  final implicit def downcastValueLocally[P <: Peer, T](v: T `local on` P)
    (implicit ev: LocalPeer[P]): T on P = `#macro`
}
