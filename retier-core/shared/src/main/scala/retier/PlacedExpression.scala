package retier

import dslparadise._

protected final abstract class OverridingExpression[P <: Peer] {
  def overriding(declaration: LocalDeclaration): PlacingExpression[P]
}

protected final abstract class SpecialPlacingExpression[P <: Peer] {
  def `abstract`[T]
    (implicit ev: NoLocalPeer[_]): T on P = `#macro`
  def base[T <: (_ localOn _), U, P0 <: Peer](v: T)
    (implicit
        ev0: LocalPeer[P],
        ev1: T <:< (_ localOn P0),
        ev2: P <:< P0,
        ev3: LocalValueTypes[T, U]): U = `#macro`
  def main(f: CurrentLocalPeer[P] `implicit =>` Unit `argument name` { type ! })
    (implicit ev: NoLocalPeer[_]): Unit on P = `#macro`
  def terminating(f: CurrentLocalPeer[P] `implicit =>` Unit `argument name` { type ! })
    (implicit ev: NoLocalPeer[_]): Unit on P = `#macro`
  def error(f: CurrentLocalPeer[P] `implicit =>` Unit `argument name` { type ! })
    (implicit ev: NoLocalPeer[_]): Unit on P = `#macro`
  def fatal(f: CurrentLocalPeer[P] `implicit =>` Unit `argument name` { type ! })
    (implicit ev: NoLocalPeer[_]): Unit on P = `#macro`
}

protected final abstract class PlacingExpression[P <: Peer] {
  def apply[T, U, V](f: CurrentLocalPeer[P] `implicit =>` T `argument name` { type ! })
    (implicit
        ev0: NoLocalPeer[_],
        ev1: PlacingTypes[P, T, U],
        ev2: LocalValueTypes[U, V]): V on P
  def shared[T, U, V](f: CurrentLocalPeer[P] `implicit =>` T `argument name` { type ! })
    (implicit
        ev0: NoLocalPeer[_],
        ev1: PlacingTypes[P, T, U],
        ev2: LocalValueTypes[U, V]): V on P
  def local[T, U, V](f: CurrentLocalPeer[P] `implicit =>` T `argument name` { type ! })
    (implicit
        ev0: NoLocalPeer[_],
        ev1: PlacingTypes[P, T, U],
        ev2: LocalValueTypes[U, V]): V localOn P
  def issued[R <: Peer]: IssuingExpression[P, R]
}

protected final abstract class IssuingExpression[P <: Peer, R <: Peer] {
  def apply[T, U, V, I](f: CurrentLocalPeer[P] `implicit =>` T `argument name` { type ! })
    (implicit
        ev0: NoLocalPeer[_],
        ev1: PlacingTypes[P, T, I],
        ev2: IssuingTypes[R, I, U],
        ev3: LocalValueTypes[U, V],
        ev4: PeerConnection[P#Connection, R, _]): V on P
}
