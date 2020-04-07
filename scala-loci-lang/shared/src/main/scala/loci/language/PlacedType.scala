package loci
package language

import scala.language.experimental.macros
import scala.language.higherKinds


sealed trait PeerType[Q, R, P]

sealed trait PeerTypeDefault {
  implicit def default[P, R]: PeerType[P, R, P] = erased
}

object PeerType extends PeerTypeDefault {
  implicit def nothing[P]: PeerType[Nothing, P, P] = erased
}


sealed trait PlacedType[T, U]

sealed trait PlacedTypeDefault {
  implicit def default[T]: PlacedType[T, T] = erased
}

sealed trait PlacedTypeSubjective extends PlacedTypeDefault {
  implicit def subjective[T, P]: PlacedType[Remote[P] => T, T per P] = erased
}

object PlacedType extends PlacedTypeSubjective {
  implicit def nothing: PlacedType[Nothing, Nothing] = erased
}


sealed trait PlacedClean[+V, L, T, T_, +U]

sealed trait PlacedCleanFallback {
  implicit def fallback[V, L, T, U]: PlacedClean[V, L, T, T, U] = erased
}

sealed trait PlacedCleanDefault extends PlacedCleanFallback {
  implicit def default[V, L, T]: PlacedClean[V, L, T, T, T] = erased
}

sealed trait PlacedCleanHigherKind8 extends PlacedCleanDefault {
  implicit def higherKind8[V, L, T[_, _, _, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, T6, T7, U0, U1, U2, U3, U4, U5, U6, U7]
    (implicit
        ev0: T[T0, T1, T2, T3, T4, T5, T6, T7] =:= T_,
        ev1: PlacedClean[V, L, T0, T0, U0],
        ev2: PlacedClean[V, L, T1, T1, U1],
        ev3: PlacedClean[V, L, T2, T2, U2],
        ev4: PlacedClean[V, L, T3, T3, U3],
        ev5: PlacedClean[V, L, T4, T4, U4],
        ev6: PlacedClean[V, L, T5, T5, U5],
        ev7: PlacedClean[V, L, T6, T6, U6],
        ev8: PlacedClean[V, L, T7, T7, U7])
    : PlacedClean[V, L, T[T0, T1, T2, T3, T4, T5, T6, T7], T_, T[U0, U1, U2, U3, U4, U5, U6, U7]] = erased(ev0, ev1, ev2, ev3, ev4, ev5, ev6, ev7, ev8)
}

sealed trait PlacedCleanHigherKind7 extends PlacedCleanHigherKind8 {
  implicit def higherKind7[V, L, T[_, _, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, T6, U0, U1, U2, U3, U4, U5, U6]
    (implicit
        ev0: T[T0, T1, T2, T3, T4, T5, T6] =:= T_,
        ev1: PlacedClean[V, L, T0, T0, U0],
        ev2: PlacedClean[V, L, T1, T1, U1],
        ev3: PlacedClean[V, L, T2, T2, U2],
        ev4: PlacedClean[V, L, T3, T3, U3],
        ev5: PlacedClean[V, L, T4, T4, U4],
        ev6: PlacedClean[V, L, T5, T5, U5],
        ev7: PlacedClean[V, L, T6, T6, U6])
    : PlacedClean[V, L, T[T0, T1, T2, T3, T4, T5, T6], T_, T[U0, U1, U2, U3, U4, U5, U6]] = erased(ev0, ev1, ev2, ev3, ev4, ev5, ev6, ev7)
}

sealed trait PlacedCleanHigherKind6 extends PlacedCleanHigherKind7 {
  implicit def higherKind6[V, L, T[_, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, U0, U1, U2, U3, U4, U5]
    (implicit
        ev0: T[T0, T1, T2, T3, T4, T5] =:= T_,
        ev1: PlacedClean[V, L, T0, T0, U0],
        ev2: PlacedClean[V, L, T1, T1, U1],
        ev3: PlacedClean[V, L, T2, T2, U2],
        ev4: PlacedClean[V, L, T3, T3, U3],
        ev5: PlacedClean[V, L, T4, T4, U4],
        ev6: PlacedClean[V, L, T5, T5, U5])
    : PlacedClean[V, L, T[T0, T1, T2, T3, T4, T5], T_, T[U0, U1, U2, U3, U4, U5]] = erased(ev0, ev1, ev2, ev3, ev4, ev5, ev6)
}

sealed trait PlacedCleanHigherKind5 extends PlacedCleanHigherKind6 {
  implicit def higherKind5[V, L, T[_, _, _, _, _], T_, T0, T1, T2, T3, T4, U0, U1, U2, U3, U4]
    (implicit
        ev0: T[T0, T1, T2, T3, T4] =:= T_,
        ev1: PlacedClean[V, L, T0, T0, U0],
        ev2: PlacedClean[V, L, T1, T1, U1],
        ev3: PlacedClean[V, L, T2, T2, U2],
        ev4: PlacedClean[V, L, T3, T3, U3],
        ev5: PlacedClean[V, L, T4, T4, U4])
    : PlacedClean[V, L, T[T0, T1, T2, T3, T4], T_, T[U0, U1, U2, U3, U4]] = erased(ev0, ev1, ev2, ev3, ev4, ev5)
}

sealed trait PlacedCleanHigherKind4 extends PlacedCleanHigherKind5 {
  implicit def higherKind4[V, L, T[_, _, _, _], T_, T0, T1, T2, T3, U0, U1, U2, U3]
    (implicit
        ev0: T[T0, T1, T2, T3] =:= T_,
        ev1: PlacedClean[V, L, T0, T0, U0],
        ev2: PlacedClean[V, L, T1, T1, U1],
        ev3: PlacedClean[V, L, T2, T2, U2],
        ev4: PlacedClean[V, L, T3, T3, U3])
    : PlacedClean[V, L, T[T0, T1, T2, T3], T_, T[U0, U1, U2, U3]] = erased(ev0, ev1, ev2, ev3, ev4)
}

sealed trait PlacedCleanHigherKind3 extends PlacedCleanHigherKind4 {
  implicit def higherKind3[V, L, T[_, _, _], T_, T0, T1, T2, U0, U1, U2]
    (implicit
        ev0: T[T0, T1, T2] =:= T_,
        ev1: PlacedClean[V, L, T0, T0, U0],
        ev2: PlacedClean[V, L, T1, T1, U1],
        ev3: PlacedClean[V, L, T2, T2, U2])
    : PlacedClean[V, L, T[T0, T1, T2], T_, T[U0, U1, U2]] = erased(ev0, ev1, ev2, ev3)
}

sealed trait PlacedCleanHigherKind2 extends PlacedCleanHigherKind3 {
  implicit def higherKind2[V, L, T[_, _], T_, T0, T1, U0, U1]
    (implicit
        ev0: T[T0, T1] =:= T_,
        ev1: PlacedClean[V, L, T0, T0, U0],
        ev2: PlacedClean[V, L, T1, T1, U1])
    : PlacedClean[V, L, T[T0, T1], T_, T[U0, U1]] = erased(ev0, ev1, ev2)
}

sealed trait PlacedCleanHigherKind1 extends PlacedCleanHigherKind2 {
  implicit def higherKind1[V, L, T[_], T_, T0, U0]
    (implicit
        ev0: T[T0] =:= T_,
        ev1: PlacedClean[V, L, T0, T0, U0])
    : PlacedClean[V, L, T[T0], T_, T[U0]] = erased(ev0, ev1)
}

sealed trait PlacedCleanRemotePeerSelection extends PlacedCleanHigherKind1 {
  implicit def remotePeerSelection[V, L, T, P]
    : PlacedClean[V, L, T from P, T from P, Unit] = erased
  implicit def remotePeerSelectionSingle[V, L, T, P]
    : PlacedClean[V, L, T fromSingle P, T fromSingle P, Unit] = erased
  implicit def remotePeerSelectionMultiple[V, L, T, P]
    : PlacedClean[V, L, T fromMultiple P, T fromMultiple P, Unit] = erased
}

sealed trait PlacedCleanRemotePeer extends PlacedCleanRemotePeerSelection {
  implicit def remotePeer[V, L, T, P]
    : PlacedClean[V, L, T on P, T on P, Unit] = erased
}

sealed trait PlacedCleanLocalPeer extends PlacedCleanRemotePeer {
  implicit def localPeerLocal[V, L <: P, T, U, P](implicit ev: PlacedClean[V, L, T, T, U])
    : PlacedClean[V, L, Local[T] on P, Local[T] on P, U] = erased(ev)
}

sealed trait PlacedCleanSubjective extends PlacedCleanLocalPeer {
  implicit def subjectivePeer[V, L, T, P, R]
    : PlacedClean[V, L, T per R on P, T per R on P, Unit] = erased
}

sealed trait PlacedCleanAny extends PlacedCleanSubjective {
  implicit def any[V, L]
    : PlacedClean[V, L, Any, Any, Any] = erased
}

sealed trait PlacedCleanNothingSubjective extends PlacedCleanAny {
  implicit def nothing[V, L]
    : PlacedClean[V, L, Nothing, Nothing, Nothing] = erased
  implicit def subjective[V, L, T, U, R](implicit ev: PlacedClean[Nothing on Nothing, L, T, T, U])
    : PlacedClean[V, L, T per R, T per R, U per R] = erased(ev)
}

object PlacedClean extends PlacedCleanNothingSubjective {
  implicit def macroGenerated[V, L, T, U](implicit ev: MacroGenerated[V on L, L, T, T, U])
    : PlacedClean[V on L, L, T, T, U] = erased(ev)


  sealed trait MacroGenerated[+V, L, T, T_, +U]

  object MacroGenerated {
    implicit def macroGenerated[V, L, T, U]: MacroGenerated[V on L, L, T, T, U] =
      macro impl.PlacedType[V, L, T]

    implicit def macroGeneratedAmbiguous[V, L, T, U]: MacroGenerated[V on L, L, T, T, U] =
      macro impl.PlacedType.macroExpansion
  }
}
