package loci
package embedding

import loci.language.{on => _, _}


sealed trait PeerType[Q, R, P]

sealed trait PeerTypeDefault:
  given default[P, R]: PeerType[P, R, P] = erased

object PeerType extends PeerTypeDefault:
  given nothing[P]: PeerType[Nothing, P, P] = erased


sealed trait PlacedType[T, U]

sealed trait PlacedTypeDefault:
  given default[T]: PlacedType[T, T] = erased

sealed trait PlacedTypeSubjective extends PlacedTypeDefault:
  given subjective[T, P]: PlacedType[Remote[P] => T, T per P] = erased

object PlacedType extends PlacedTypeSubjective:
  given nothing: PlacedType[Nothing, Nothing] = erased


sealed trait CanonicalPlacedTypeAlias[T, U]

sealed trait CanonicalPlacedTypeAliasNonSelected:
//  given onLanguage[T, P]: CanonicalPlacedTypeAlias[T _on_ P, T _on_ P] = erased
  given on[T, P]: CanonicalPlacedTypeAlias[T on P, T on P] = erased
  given from[T, P]: CanonicalPlacedTypeAlias[T from P, T from P] = erased

object CanonicalPlacedTypeAlias extends CanonicalPlacedTypeAliasNonSelected:
  given fromSingle[T, P]: CanonicalPlacedTypeAlias[T fromSingle P, T fromSingle P] = erased
  given fromMultiple[T, P]: CanonicalPlacedTypeAlias[T fromMultiple P, T fromMultiple P] = erased


sealed trait PlacedClean[+V, L, T, T_, +U]

sealed trait PlacedCleanFallback:
  given fallback[V, L, T, U]: PlacedClean[V, L, T, T, U] = erased

sealed trait PlacedCleanDefault extends PlacedCleanFallback:
  given default[V, L, T]: PlacedClean[V, L, T, T, T] = erased

sealed trait PlacedCleanHigherKind1 extends PlacedCleanDefault:
  given higherKind1[V, L, T[_], T_, T0, U0](using
    T[T0] =:= T_,
    PlacedClean[V, L, T0, T0, U0])
  : PlacedClean[V, L, T[T0], T_, T[U0]] = erased

sealed trait PlacedCleanHigherKind2 extends PlacedCleanHigherKind1:
  given higherKind2[V, L, T[_, _], T_, T0, T1, U0, U1](using
    T[T0, T1] =:= T_,
    PlacedClean[V, L, T0, T0, U0],
    PlacedClean[V, L, T1, T1, U1])
  : PlacedClean[V, L, T[T0, T1], T_, T[U0, U1]] = erased

sealed trait PlacedCleanHigherKind3 extends PlacedCleanHigherKind2:
  given higherKind3[V, L, T[_, _, _], T_, T0, T1, T2, U0, U1, U2](using
    T[T0, T1, T2] =:= T_,
    PlacedClean[V, L, T0, T0, U0],
    PlacedClean[V, L, T1, T1, U1],
    PlacedClean[V, L, T2, T2, U2])
  : PlacedClean[V, L, T[T0, T1, T2], T_, T[U0, U1, U2]] = erased

sealed trait PlacedCleanHigherKind4 extends PlacedCleanHigherKind3:
  given higherKind4[V, L, T[_, _, _, _], T_, T0, T1, T2, T3, U0, U1, U2, U3](using
    T[T0, T1, T2, T3] =:= T_,
    PlacedClean[V, L, T0, T0, U0],
    PlacedClean[V, L, T1, T1, U1],
    PlacedClean[V, L, T2, T2, U2],
    PlacedClean[V, L, T3, T3, U3])
  : PlacedClean[V, L, T[T0, T1, T2, T3], T_, T[U0, U1, U2, U3]] = erased

sealed trait PlacedCleanHigherKind5 extends PlacedCleanHigherKind4:
  given higherKind5[V, L, T[_, _, _, _, _], T_, T0, T1, T2, T3, T4, U0, U1, U2, U3, U4](using
    T[T0, T1, T2, T3, T4] =:= T_,
    PlacedClean[V, L, T0, T0, U0],
    PlacedClean[V, L, T1, T1, U1],
    PlacedClean[V, L, T2, T2, U2],
    PlacedClean[V, L, T3, T3, U3],
    PlacedClean[V, L, T4, T4, U4])
  : PlacedClean[V, L, T[T0, T1, T2, T3, T4], T_, T[U0, U1, U2, U3, U4]] = erased


sealed trait PlacedCleanHigherKind6 extends PlacedCleanHigherKind5:
  given higherKind6[V, L, T[_, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, U0, U1, U2, U3, U4, U5](using
    T[T0, T1, T2, T3, T4, T5] =:= T_,
    PlacedClean[V, L, T0, T0, U0],
    PlacedClean[V, L, T1, T1, U1],
    PlacedClean[V, L, T2, T2, U2],
    PlacedClean[V, L, T3, T3, U3],
    PlacedClean[V, L, T4, T4, U4],
    PlacedClean[V, L, T5, T5, U5])
  : PlacedClean[V, L, T[T0, T1, T2, T3, T4, T5], T_, T[U0, U1, U2, U3, U4, U5]] = erased

sealed trait PlacedCleanHigherKind7 extends PlacedCleanHigherKind6:
  given higherKind7[V, L, T[_, _, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, T6, U0, U1, U2, U3, U4, U5, U6](using
    T[T0, T1, T2, T3, T4, T5, T6] =:= T_,
    PlacedClean[V, L, T0, T0, U0],
    PlacedClean[V, L, T1, T1, U1],
    PlacedClean[V, L, T2, T2, U2],
    PlacedClean[V, L, T3, T3, U3],
    PlacedClean[V, L, T4, T4, U4],
    PlacedClean[V, L, T5, T5, U5],
    PlacedClean[V, L, T6, T6, U6])
  : PlacedClean[V, L, T[T0, T1, T2, T3, T4, T5, T6], T_, T[U0, U1, U2, U3, U4, U5, U6]] = erased

sealed trait PlacedCleanHigherKind8 extends PlacedCleanHigherKind7:
  given higherKind8[V, L, T[_, _, _, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, T6, T7, U0, U1, U2, U3, U4, U5, U6, U7](using
    T[T0, T1, T2, T3, T4, T5, T6, T7] =:= T_,
    PlacedClean[V, L, T0, T0, U0],
    PlacedClean[V, L, T1, T1, U1],
    PlacedClean[V, L, T2, T2, U2],
    PlacedClean[V, L, T3, T3, U3],
    PlacedClean[V, L, T4, T4, U4],
    PlacedClean[V, L, T5, T5, U5],
    PlacedClean[V, L, T6, T6, U6],
    PlacedClean[V, L, T7, T7, U7])
  : PlacedClean[V, L, T[T0, T1, T2, T3, T4, T5, T6, T7], T_, T[U0, U1, U2, U3, U4, U5, U6, U7]] = erased

sealed trait PlacedCleanRemotePeerSelection extends PlacedCleanHigherKind8:
  given remotePeerSelection[V, L, T, P]: PlacedClean[V, L, T from P, T from P, Unit] = erased
  given remotePeerSelectionSingle[V, L, T, P]: PlacedClean[V, L, T fromSingle P, T fromSingle P, Unit] = erased
  given remotePeerSelectionMultiple[V, L, T, P]: PlacedClean[V, L, T fromMultiple P, T fromMultiple P, Unit] = erased

sealed trait PlacedCleanRemotePeer extends PlacedCleanRemotePeerSelection:
//  given remotePeerLanguage[V, L, T, P]: PlacedClean[V, L, T _on_ P, T _on_ P, Unit] = erased
  given remotePeer[V, L, T, P]: PlacedClean[V, L, T on P, T on P, Unit] = erased

sealed trait PlacedCleanLocalPeer extends PlacedCleanRemotePeer:
//  given localPeerLanguage[V, L <: P, T, U, P](using PlacedClean[V, L, T, T, U]): PlacedClean[V, L, T _on_ P, T _on_ P, U] = erased
  given localPeer[V, L <: P, T, U, P](using PlacedClean[V, L, T, T, U]): PlacedClean[V, L, T on P, T on P, U] = erased

sealed trait PlacedCleanLocalPeerLocal extends PlacedCleanLocalPeer:
//  given localPeerLocalLanguage[V, L <: P, T, U, P, _Local_[T] <: Local[T]](using
//    _Local_[T] =:= T,
//    PlacedClean[V, L, T, T, U])
//    : PlacedClean[V, L, _Local_[T] _on_ P, _Local_[T] _on_ P, U] = erased
  given localPeerLocal[V, L <: P, T, U, P, _Local_[T] <: Local[T]](using
    _Local_[T] =:= T,
    PlacedClean[V, L, T, T, U])
  : PlacedClean[V, L, _Local_[T] on P, _Local_[T] on P, U] = erased

sealed trait PlacedCleanSubjective extends PlacedCleanLocalPeerLocal:
//  given subjectivePeerLanguage[V, L, T, P, R]: PlacedClean[V, L, T per R _on_ P, T per R _on_ P, Unit] = erased
  given subjectivePeer[V, L, T, P, R]: PlacedClean[V, L, T per R on P, T per R on P, Unit] = erased

sealed trait PlacedCleanAny extends PlacedCleanSubjective:
  given any[V, L]: PlacedClean[V, L, Any, Any, Any] = erased

sealed trait PlacedCleanNothingSubjective extends PlacedCleanAny:
  given nothing[V, L]: PlacedClean[V, L, Nothing, Nothing, Nothing] = erased

object PlacedClean extends PlacedCleanNothingSubjective
//  implicit def macroGenerated[V, L, T, U](implicit ev: MacroGenerated[V on L, L, T, T, U])
//    : PlacedClean[V on L, L, T, T, U] = erased(ev)
//
//
//  sealed trait MacroGenerated[+V, L, T, T_, +U]
//
//  object MacroGenerated {
//    implicit def macroGenerated[V, L, T, U]: MacroGenerated[V on L, L, T, T, U] =
//      macro impl.PlacedType[V, L, T]
//
//    implicit def macroGeneratedAmbiguous[V, L, T, U]: MacroGenerated[V on L, L, T, T, U] =
//      macro impl.PlacedType.macroExpansion
//  }
