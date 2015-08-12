package retier

import typeconstraints._
import scala.annotation.implicitNotFound

@implicitNotFound("Expression not placeable on peer ${P}.")
private final abstract class PlacingTypes[P <: Peer, T, U]

private object PlacingTypes {
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

@implicitNotFound("Issued type not inferable.")
private final abstract class IssuingTypes[R <: Peer, T, U]

private object IssuingTypes {
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
