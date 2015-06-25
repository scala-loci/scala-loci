package retier

import dslparadise._
import scala.language.higherKinds

protected final abstract class RemoteSelectionExpression {
  def apply[P <: Peer](peer: Remote[P]):
    RemoteExpression[P, `from single`] = `#macro`
  def apply[P <: Peer](peers: Remote[P]*):
    RemoteExpression[P, `from multiple`] = `#macro`
}

protected final abstract class RemoteExpression[P <: Peer, placed[_, _ <: Peer]]
    extends RemoteExpressionUsing[P, placed] {
  def apply[T, U, L <: Peer](f: CurrentLocalPeerRemoteComputation[P] `implicit =>` T)
    (implicit
        ev0: LocalPeer[L],
        ev1: PlacingTypes[P, T, U],
        ev2: PeerConnection[L#Connection, P, _]): U placed P = `#macro`
  def using[T, U, L <: Peer, V0]
    (v0: V0)
    (f: CurrentLocalPeerRemoteComputation[P] `implicit =>` (
      Captured[V0] => T))
    (implicit
        ev0: LocalPeer[L],
        ev1: PlacingTypes[P, T, U],
        ev2: PeerConnection[L#Connection, P, _]): U placed P = `#macro`
  def execute[T, L <: Peer](f: Unit => T)
    (implicit
        ev0: LocalPeer[L],
        ev2: PeerConnection[L#Connection, P, _]): T placed P = `#macro`
  def call[T, L <: Peer](method: RemoteMethod[T, P])
    (implicit
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, P, _]): T placed P = `#macro`
  def set[T, L <: Peer](property: RemoteProperty[T, P])
    (implicit
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, P, _]): RemoteSettingExpression[T, P, L] = `#macro`
  def issued: RemoteIssuingExpression[P, placed] = `#macro`
}

protected final abstract class RemoteSettingExpression[T, P <: Peer, L <: Peer] {
  def to(v: T `local on` L): Unit on P = `#macro`
}

protected final abstract class RemoteIssuingExpression[P <: Peer, placed[_, _ <: Peer]]
    extends RemoteExpressionIssuedUsing[P, placed] {
  def apply[T, U, I, L <: Peer](f: CurrentLocalPeerRemoteComputation[P] `implicit =>` T)
    (implicit
        ev0: LocalPeer[L],
        ev1: PlacingTypes[P, T, I],
        ev2: IssuingTypes[L, I, U],
        ev3: PeerConnection[L#Connection, P, _],
        ev4: PeerConnection[P#Connection, L, _]): U placed P = `#macro`
  def using[T, U, I, L <: Peer, V0]
    (v0: V0)
    (f: CurrentLocalPeerRemoteComputation[P] `implicit =>` (
      Captured[V0] => T))
    (implicit
        ev0: LocalPeer[L],
        ev1: PlacingTypes[P, T, I],
        ev2: IssuingTypes[L, I, U],
        ev3: PeerConnection[L#Connection, P, _],
        ev4: PeerConnection[P#Connection, L, _]): U placed P = `#macro`
  def execute[T, U, L <: Peer](f: Unit => T)
    (implicit
        ev0: LocalPeer[L],
        ev1: IssuingTypes[L, T, U],
        ev2: PeerConnection[L#Connection, P, _]): U placed P = `#macro`
  def call[T, U, L <: Peer](method: RemoteMethod[T, P])
    (implicit
        ev0: LocalPeer[L],
        ev1: IssuingTypes[L, T, U],
        ev2: PeerConnection[L#Connection, P, _]): U placed P = `#macro`
}
