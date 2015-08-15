package retier

import dslparadise._

protected final abstract class OverridingExpression[P <: Peer] {
  def overriding(declaration: LocalDeclaration): PlacingExpression[P] = `#macro`
}

protected final abstract class PlacingExpression[P <: Peer] {
  def apply[T, U](f: CurrentLocalPeer[P] `implicit =>` T)
    (implicit
        ev0: NoLocalPeer[_],
        ev1: PlacingTypes[P, T, U]): U on P = `#macro`
  def shared[T, U](f: CurrentLocalPeer[P] `implicit =>` T)
    (implicit
        ev0: NoLocalPeer[_],
        ev1: PlacingTypes[P, T, U]): U on P = `#macro`
  def local[T, U](f: CurrentLocalPeer[P] `implicit =>` T)
    (implicit
        ev0: NoLocalPeer[_],
        ev1: PlacingTypes[P, T, U]): U localOn P = `#macro`
  def issued[R <: Peer]: IssuingExpression[P, R] = `#macro`
}

protected final abstract class IssuingExpression[P <: Peer, R <: Peer] {
  def apply[T, U, I](f: CurrentLocalPeer[P] `implicit =>` T)
    (implicit
        ev0: NoLocalPeer[_],
        ev1: PlacingTypes[P, T, I],
        ev2: IssuingTypes[R, I, U],
        ev3: PeerConnection[P#Connection, R, _]): U on P = `#macro`
}
