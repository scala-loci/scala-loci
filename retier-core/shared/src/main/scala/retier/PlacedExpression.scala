package retier

import dslparadise._

protected final abstract class OverridingExpression[P <: Peer] {
  def overriding(declaration: LocalDeclaration): PlacingExpression[P]
}

protected final abstract class SpecialPlacingExpression[P <: Peer] {
  def `abstract`[T]
    (implicit ev: NoLocalPeer[_]): T on P = `#macro`
  def base[T <: (_ localOn _), U](v: T)
    (implicit
        ev0: LocalPeer[P],
        ev1: ValueTypes[T, _, _, U]): U = `#macro`
  def main(f: CurrentLocalPeer[P] `implicit =>` Unit)
    (implicit ev: NoLocalPeer[_]): Unit on P = `#macro`
  def terminating(f: CurrentLocalPeer[P] `implicit =>` Unit)
    (implicit ev: NoLocalPeer[_]): Unit on P = `#macro`
  def error(f: CurrentLocalPeer[P] `implicit =>` Unit)
    (implicit ev: NoLocalPeer[_]): Unit on P = `#macro`
  def fatal(f: CurrentLocalPeer[P] `implicit =>` Unit)
    (implicit ev: NoLocalPeer[_]): Unit on P = `#macro`
}

protected final abstract class PlacingExpression[P <: Peer] {
  def apply[T, U](f: CurrentLocalPeer[P] `implicit =>` T)
    (implicit
        ev0: NoLocalPeer[_],
        ev1: PlacingTypes[P, T, U]): U on P
  def shared[T, U](f: CurrentLocalPeer[P] `implicit =>` T)
    (implicit
        ev0: NoLocalPeer[_],
        ev1: PlacingTypes[P, T, U]): U on P
  def local[T, U](f: CurrentLocalPeer[P] `implicit =>` T)
    (implicit
        ev0: NoLocalPeer[_],
        ev1: PlacingTypes[P, T, U]): U localOn P
  def issued[R <: Peer]: IssuingExpression[P, R]
}

protected final abstract class IssuingExpression[P <: Peer, R <: Peer] {
  def apply[T, U, I](f: CurrentLocalPeer[P] `implicit =>` T)
    (implicit
        ev0: NoLocalPeer[_],
        ev1: PlacingTypes[P, T, I],
        ev2: IssuingTypes[R, I, U],
        ev3: PeerConnection[P#Connection, R, _]): U on P
}
