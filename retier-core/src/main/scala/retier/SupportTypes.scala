package retier

import typeconstraints._
import scala.annotation.implicitNotFound

@implicitNotFound("Expression not placeable on peer ${P}")
protected final abstract class PlacingTypes[P <: Peer, T, U]

protected object PlacingTypes {
  implicit def localPlacedType[P <: Peer, P0 <: Peer, T, U]
    (implicit
        ev0: T <:< (U localOn P0),
        ev1: P <:< P0): PlacingTypes[P, T, U] = `#macro`
  implicit def remotePlacedType[P <: Peer, P0 <: Peer, T, U]
    (implicit
        ev0: T <:< (U localOn P0),
        ev1: P <:!< P0): PlacingTypes[P, T, Unit] = `#macro`
  implicit def nonPlacedType[P <: Peer, T]
    (implicit
        ev: T <:!< (_ localOn _)): PlacingTypes[P, T, T] = `#macro`
}

@implicitNotFound("Issued type not inferable")
protected final abstract class IssuingTypes[R <: Peer, T, U]

protected object IssuingTypes {
  implicit def controlledIssuedType[R <: Peer, T, U]
    (implicit
        ev0: T <:< (Remote[R] => U),
        ev1: T <:!< (Remote[R] <-> U)): IssuingTypes[R, T, Remote[R] <=> U] = `#macro`
  implicit def issuedTypePassed[R <: Peer, T, U]
    (implicit
        ev: T <:< (Remote[R] <-> U)): IssuingTypes[R, T, Remote[R] <-> U] = `#macro`
  implicit def issuedTypeLifted[R <: Peer, T]
    (implicit
        ev: T <:!< (_ => _)): IssuingTypes[R, T, Remote[R] <-> T] = `#macro`
}

@implicitNotFound("Value not accessible remotely")
protected final abstract class ValueTypes[T, U]

protected object ValueTypes {
  implicit def placedValueNonIssued[P <: Peer, T, U]
    (implicit
        ev0: T <:< (U localOn P),
        ev1: LocalPeer[P],
        ev2: U <:!< (_ <=> _)): ValueTypes[T, U] = `#macro`
  implicit def placedValueControlledIssued[P <: Peer, R <: Remote[Peer], T, U, V]
    (implicit
        ev0: T <:< (U localOn P),
        ev1: LocalPeer[P],
        ev2: U <:< (R <=> V), ev3: U <:!< (_ <-> _)): ValueTypes[T, R => V] = `#macro`
  implicit def placedValueIssued[P <: Peer, T, U, V]
    (implicit
        ev0: T <:< (U localOn P),
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
        ev2: U <:!< (_ localOn _)): ValueTypes[T, U] = `#macro`
  implicit def capturedValuePlaced[T, U, V]
    (implicit
        ev0: T <:< Captured[U],
        ev1: U <:< (V localOn _)): ValueTypes[T, V] = `#macro`
}
