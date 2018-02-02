package loci

import dslparadise._
import scala.language.higherKinds

protected final abstract class RemoteSelectionExpression[P <: Peer] {
  def on[P0 <: P](peer: Remote[P0]): RemoteExpression[P, fromSingle]
  def on[P0 <: P](peers: Remote[P0]*): RemoteExpression[P, fromMultiple]
}

protected final abstract class RemoteExpression[P <: Peer, placed[_, _ <: Peer]] {
  def apply[T, U, V, L <: Peer](f: CurrentLocalPeerRemoteComputation[P] `implicit =>` T `argument name` { type ! })
    (implicit
        ev0: LocalPeer[L],
        ev1: RemotePlacingTypes[T, U],
        ev2: LocalValueTypes[U, V],
        ev3: PeerTie[L#Tie, P, _]): V placed P
  def capture
    (v: LocalDeclaration*): RemoteCapturingExpression[P, placed]
  def call[T, L <: Peer, P0 >: P <: Peer, `P'` <: Peer](method: RemoteMethod[T, P0])
    (implicit
        ev0: LocalPeer[L],
        ev1: FirstIfNotEmptyElseSecond[P, P0, `P'`],
        ev2: PeerTie[L#Tie, `P'`, _]): T placed `P'`
  def set[T, L <: Peer, P0 >: P <: Peer, `P'` <: Peer](property: RemoteProperty[T, P0])
    (implicit
        ev0: LocalPeer[L],
        ev1: FirstIfNotEmptyElseSecond[P, P0, `P'`],
        ev2: PeerTie[L#Tie, `P'`, _]): RemoteSettingExpression[T, `P'`, L, placed]
  def sbj: RemoteSubjectiveExpression[P, placed]
}

protected final abstract class RemoteSettingExpression[T, P <: Peer, L <: Peer, placed[_, _ <: Peer]] {
  def :=(v: T): Unit placed P
}

protected final abstract class RemoteCapturingExpression[P <: Peer, placed[_, _ <: Peer]] {
  def apply[T, U, V, L <: Peer]
    (f: CurrentLocalPeerRemoteComputation[P] `implicit =>` T `argument name` { type ! })
    (implicit
        ev0: LocalPeer[L],
        ev1: RemotePlacingTypes[T, U],
        ev2: LocalValueTypes[U, V],
        ev3: PeerTie[L#Tie, P, _]): V placed P
}

protected final abstract class RemoteSubjectiveExpression[P <: Peer, placed[_, _ <: Peer]] {
  def apply[T, U, V, I, L <: Peer](f: CurrentLocalPeerRemoteComputation[P] `implicit =>` T `argument name` { type ! })
    (implicit
        ev0: LocalPeer[L],
        ev1: RemotePlacingTypes[T, I],
        ev2: SubjectiveTypes[L, I, U],
        ev3: LocalValueTypes[U, V],
        ev4: PeerTie[L#Tie, P, _],
        ev5: PeerTie[P#Tie, L, _]): V placed P
  def capture[T, U, I, L <: Peer]
    (v: LocalDeclaration*): RemoteSubjectiveCapturingExpression[P, placed]
}

protected final abstract class RemoteSubjectiveCapturingExpression[P <: Peer, placed[_, _ <: Peer]] {
  def apply[T, U, V, I, L <: Peer]
    (f: CurrentLocalPeerRemoteComputation[P] `implicit =>` T `argument name` { type ! })
    (implicit
        ev0: LocalPeer[L],
        ev1: RemotePlacingTypes[T, I],
        ev2: SubjectiveTypes[L, I, U],
        ev3: LocalValueTypes[U, V],
        ev4: PeerTie[L#Tie, P, _],
        ev5: PeerTie[P#Tie, L, _]): V placed P
}


protected final abstract class FirstIfNotEmptyElseSecond[N, O, T]

protected sealed trait FirstIfNotEmptyElseSecondSecondFallback {
  implicit def nothingOrNotInferred[N, O]:
    FirstIfNotEmptyElseSecond[N, O, O] = `#macro`
}

protected sealed trait FirstIfNotEmptyElseSecondFirstFallback
    extends FirstIfNotEmptyElseSecondSecondFallback {
  implicit def inferred[N, O]
    (implicit ev: N =:= N): FirstIfNotEmptyElseSecond[N, O, N] = `#macro`(ev)
}

protected object FirstIfNotEmptyElseSecond
    extends FirstIfNotEmptyElseSecondFirstFallback {
  implicit def nothing[N, O]
    (implicit ev: N =:= Nothing): FirstIfNotEmptyElseSecond[N, O, O] = `#macro`(ev)
}
