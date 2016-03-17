package retier

import typeconstraints._
import scala.annotation.implicitNotFound

@implicitNotFound("Expression not placeable on peer")
protected final abstract class PlacingTypes[P <: Peer, T, U]

protected trait PlacingTypesFallback {
  implicit def nothingOrNotInferred[P <: Peer, T, NothingButLessSpecific]:
    PlacingTypes[P, T, NothingButLessSpecific] = `#macro`
}

protected object PlacingTypes extends PlacingTypesFallback {
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

@implicitNotFound("Expression not placeable on peer")
protected final abstract class RemotePlacingTypes[T, U]

protected object RemotePlacingTypes {
  implicit def placedType[T, U]
    (implicit
        ev: T <:< (U localOn _)): RemotePlacingTypes[T, U] = `#macro`
  implicit def nonPlacedType[T]
    (implicit
        ev: T <:!< (_ localOn _)): RemotePlacingTypes[T, T] = `#macro`
}

@implicitNotFound("Issued type not inferable")
protected final abstract class IssuingTypes[R <: Peer, T, U]

protected object IssuingTypes {
  implicit def issuedType[R <: Peer, T]
    (implicit
        ev0: T <:!< (Remote[R] <=> _),
        ev1: T <:!< (Remote[R] <-> _),
        ev2: T <:!< (Remote[R] => _)): IssuingTypes[R, T, Remote[R] <-> T] = `#macro`
  implicit def controlledIssuedType[R <: Peer, T, U]
    (implicit
        ev: T <:< (Remote[R] => U)): IssuingTypes[R, T, Remote[R] <=> U] = `#macro`
  implicit def issuedTypePassed[R <: Peer, T, U]
    (implicit
        ev: T <:< (Remote[R] <-> U)): IssuingTypes[R, T, Remote[R] <-> U] = `#macro`
  implicit def controlledIssuedTypePassed[R <: Peer, T, U]
    (implicit
        ev: T <:< (Remote[R] <=> U)): IssuingTypes[R, T, Remote[R] <=> U] = `#macro`
}

@implicitNotFound("Value not accessible remotely")
protected final abstract class ValueTypes[T, R <: Remote[Peer], U, V]

protected trait ValueTypesFallback {
  implicit def placedRemoteValue[T, U]
    (implicit
        ev0: T <:< (U localOn _),
        ev1: LocalPeer[_],
        ev2: CurrentLocalPeerRemoteComputation[_],
        ev3: U <:!< (_ <=> _),
        ev4: U <:!< (_ <-> _)): ValueTypes[T, Remote[Peer], U, U] = `#macro`
  implicit def placedRemoteValueControlledIssued[R <: Remote[Peer], T, U, V]
    (implicit
        ev0: T <:< (U localOn _),
        ev1: LocalPeer[_],
        ev2: CurrentLocalPeerRemoteComputation[_],
        ev3: U <:< (R <=> V)): ValueTypes[T, R, V, R => V] = `#macro`
  implicit def placedRemoteValueIssued[R <: Remote[Peer], T, U, V]
    (implicit
        ev0: T <:< (U localOn _),
        ev1: LocalPeer[_],
        ev2: CurrentLocalPeerRemoteComputation[_],
        ev3: U <:< (R <-> V)): ValueTypes[T, R, V, V] = `#macro`
}

protected object ValueTypes extends ValueTypesFallback {
  implicit def placedValue[P <: Peer, T, U]
    (implicit
        ev0: T <:< (U localOn P),
        ev1: LocalPeer[P],
        ev2: U <:!< (_ <=> _),
        ev3: U <:!< (_ <-> _)): ValueTypes[T, Remote[Peer], U, U] = `#macro`
  implicit def placedValueControlledIssued[R <: Remote[Peer], P <: Peer, T, U, V]
    (implicit
        ev0: T <:< (U localOn P),
        ev1: LocalPeer[P],
        ev2: U <:< (R <=> V)): ValueTypes[T, R, V, R => V] = `#macro`
  implicit def placedValueIssued[R <: Remote[Peer], P <: Peer, T, U, V]
    (implicit
        ev0: T <:< (U localOn P),
        ev1: LocalPeer[P],
        ev2: U <:< (R <-> V)): ValueTypes[T, R, V, V] = `#macro`
}
