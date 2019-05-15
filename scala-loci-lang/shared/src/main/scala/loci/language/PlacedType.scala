package loci
package language

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


sealed trait PlacedClean[L, T, T_, +U]

sealed trait PlacedCleanDefault {
  implicit def default[L, T]: PlacedClean[L, T, T, T] = erased
}

sealed trait PlacedCleanHigherKinds extends PlacedCleanDefault {
  implicit def higherKind1[L, T[_], T_, T0, U0]
    (implicit
        ev0: T[T0] =:= T_,
        ev1: PlacedClean[L, T0, T0, U0])
    : PlacedClean[L, T[T0], T_, T[U0]] = erased(ev0, ev1)

  implicit def higherKind2[L, T[_, _], T_, T0, T1, U0, U1]
    (implicit
        ev0: T[T0, T1] =:= T_,
        ev1: PlacedClean[L, T0, T0, U0],
        ev2: PlacedClean[L, T1, T1, U1])
    : PlacedClean[L, T[T0, T1], T_, T[U0, U1]] = erased(ev0, ev1, ev2)

  implicit def higherKind3[L, T[_, _, _], T_, T0, T1, T2, U0, U1, U2]
    (implicit
        ev0: T[T0, T1, T2] =:= T_,
        ev1: PlacedClean[L, T0, T0, U0],
        ev2: PlacedClean[L, T1, T1, U1],
        ev3: PlacedClean[L, T2, T2, U2])
    : PlacedClean[L, T[T0, T1, T2], T_, T[U0, U1, U2]] = erased(ev0, ev1, ev2, ev3)

  implicit def higherKind4[L, T[_, _, _, _], T_, T0, T1, T2, T3, U0, U1, U2, U3]
    (implicit
        ev0: T[T0, T1, T2, T3] =:= T_,
        ev1: PlacedClean[L, T0, T0, U0],
        ev2: PlacedClean[L, T1, T1, U1],
        ev3: PlacedClean[L, T2, T2, U2],
        ev4: PlacedClean[L, T3, T3, U3])
    : PlacedClean[L, T[T0, T1, T2, T3], T_, T[U0, U1, U2, U3]] = erased(ev0, ev1, ev2, ev3, ev4)

  implicit def higherKind5[L, T[_, _, _, _, _], T_, T0, T1, T2, T3, T4, U0, U1, U2, U3, U4]
    (implicit
        ev0: T[T0, T1, T2, T3, T4] =:= T_,
        ev1: PlacedClean[L, T0, T0, U0],
        ev2: PlacedClean[L, T1, T1, U1],
        ev3: PlacedClean[L, T2, T2, U2],
        ev4: PlacedClean[L, T3, T3, U3],
        ev5: PlacedClean[L, T4, T4, U4])
    : PlacedClean[L, T[T0, T1, T2, T3, T4], T_, T[U0, U1, U2, U3, U4]] = erased(ev0, ev1, ev2, ev3, ev4, ev5)

  implicit def higherKind6[L, T[_, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, U0, U1, U2, U3, U4, U5]
    (implicit
        ev0: T[T0, T1, T2, T3, T4, T5] =:= T_,
        ev1: PlacedClean[L, T0, T0, U0],
        ev2: PlacedClean[L, T1, T1, U1],
        ev3: PlacedClean[L, T2, T2, U2],
        ev4: PlacedClean[L, T3, T3, U3],
        ev5: PlacedClean[L, T4, T4, U4],
        ev6: PlacedClean[L, T5, T5, U5])
    : PlacedClean[L, T[T0, T1, T2, T3, T4, T5], T_, T[U0, U1, U2, U3, U4, U5]] = erased(ev0, ev1, ev2, ev3, ev4, ev5, ev6)

  implicit def higherKind7[L, T[_, _, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, T6, U0, U1, U2, U3, U4, U5, U6]
    (implicit
        ev0: T[T0, T1, T2, T3, T4, T5, T6] =:= T_,
        ev1: PlacedClean[L, T0, T0, U0],
        ev2: PlacedClean[L, T1, T1, U1],
        ev3: PlacedClean[L, T2, T2, U2],
        ev4: PlacedClean[L, T3, T3, U3],
        ev5: PlacedClean[L, T4, T4, U4],
        ev6: PlacedClean[L, T5, T5, U5],
        ev7: PlacedClean[L, T6, T6, U6])
    : PlacedClean[L, T[T0, T1, T2, T3, T4, T5, T6], T_, T[U0, U1, U2, U3, U4, U5, U6]] = erased(ev0, ev1, ev2, ev3, ev4, ev5, ev6, ev7)

  implicit def higherKind8[L, T[_, _, _, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, T6, T7, U0, U1, U2, U3, U4, U5, U6, U7]
    (implicit
        ev0: T[T0, T1, T2, T3, T4, T5, T6, T7] =:= T_,
        ev1: PlacedClean[L, T0, T0, U0],
        ev2: PlacedClean[L, T1, T1, U1],
        ev3: PlacedClean[L, T2, T2, U2],
        ev4: PlacedClean[L, T3, T3, U3],
        ev5: PlacedClean[L, T4, T4, U4],
        ev6: PlacedClean[L, T5, T5, U5],
        ev7: PlacedClean[L, T6, T6, U6],
        ev8: PlacedClean[L, T7, T7, U7])
    : PlacedClean[L, T[T0, T1, T2, T3, T4, T5, T6, T7], T_, T[U0, U1, U2, U3, U4, U5, U6, U7]] = erased(ev0, ev1, ev2, ev3, ev4, ev5, ev6, ev7, ev8)
}

sealed trait PlacedCleanRemotePeerSelection extends PlacedCleanHigherKinds {
  implicit def remotePeerSelection[L, T, P]
    : PlacedClean[L, T from P, T from P, Unit] = erased
  implicit def remotePeerSelectionSingle[L, T, P]
    : PlacedClean[L, T fromSingle P, T fromSingle P, Unit] = erased
  implicit def remotePeerSelectionMultiple[L, T, P]
    : PlacedClean[L, T fromMultiple P, T fromMultiple P, Unit] = erased
}

sealed trait PlacedCleanRemotePeer extends PlacedCleanRemotePeerSelection {
  implicit def remotePeer[L, T, P]
    : PlacedClean[L, T on P, T on P, Unit] = erased
}

sealed trait PlacedCleanLocalPeer extends PlacedCleanRemotePeer {
  implicit def localPeerLocal[L <: P, T, U, P](implicit ev: PlacedClean[L, T, T, U])
    : PlacedClean[L, Local[T] on P, Local[T] on P, U] = erased(ev)
}

sealed trait PlacedCleanSubjective extends PlacedCleanLocalPeer {
  implicit def subjectivePeer[L, T, P, R]
    : PlacedClean[L, T per R on P, T per R on P, Unit] = erased
}

sealed trait PlacedCleanAny extends PlacedCleanSubjective {
  implicit def any[L]
    : PlacedClean[L, Any, Any, Any] = erased
}

object PlacedClean extends PlacedCleanAny {
  implicit def nothing[L]
    : PlacedClean[L, Nothing, Nothing, Nothing] = erased
  implicit def subjective[L, T, U, R](implicit ev: PlacedClean[Nothing, T, T, U])
    : PlacedClean[L, T per R, T per R, U per R] = erased(ev)
}
