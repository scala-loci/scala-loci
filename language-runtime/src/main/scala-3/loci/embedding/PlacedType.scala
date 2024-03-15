package loci
package embedding

import loci.language.{on as _, *}
import loci.utility.reflectionExtensions.*
import utility.DummyImplicit

import scala.quoted.*


sealed trait PeerType[Q, R, P]

sealed trait PeerTypeDefault:
  given default[P, R]: PeerType[P, R, P] = erased

object PeerType extends PeerTypeDefault:
  given nothing[P]: PeerType[Nothing, P, P] = erased


sealed trait CanonicalPlacedTypeAlias[T, U]

sealed trait CanonicalPlacedTypeAliasNonSelected:
  given on0[T, P, _on_[T, P] <: T on P]: CanonicalPlacedTypeAlias[Placement.Context[P] ?=> (T _on_ P), T on P] = erased
  given on1[T, P, _on_[T, P] <: T on P]: CanonicalPlacedTypeAlias[T _on_ P, T on P] = erased
  given from[T, P]: CanonicalPlacedTypeAlias[T from P, T from P] = erased

object CanonicalPlacedTypeAlias extends CanonicalPlacedTypeAliasNonSelected:
  given fromSingle[T, P]: CanonicalPlacedTypeAlias[T fromSingle P, T fromSingle P] = erased
  given fromMultiple[T, P]: CanonicalPlacedTypeAlias[T fromMultiple P, T fromMultiple P] = erased


sealed trait PlacedClean[L, T, T_, +U]

sealed trait PlacedCleanDefault:
  given default[L, T]: PlacedClean[L, T, T, T] = erased

sealed trait PlacedCleanHigherKind1 extends PlacedCleanDefault:
  given higherKind1[L, T[_], T_, T0, U0](using
    T[T0] =:= T_,
    PlacedClean[L, T0, T0, U0])
  : PlacedClean[L, T[T0], T_, T[U0]] = erased

sealed trait PlacedCleanHigherKind2 extends PlacedCleanHigherKind1:
  given higherKind2[L, T[_, _], T_, T0, T1, U0, U1](using
    T[T0, T1] =:= T_,
    PlacedClean[L, T0, T0, U0],
    PlacedClean[L, T1, T1, U1])
  : PlacedClean[L, T[T0, T1], T_, T[U0, U1]] = erased

sealed trait PlacedCleanHigherKind3 extends PlacedCleanHigherKind2:
  given higherKind3[L, T[_, _, _], T_, T0, T1, T2, U0, U1, U2](using
    T[T0, T1, T2] =:= T_,
    PlacedClean[L, T0, T0, U0],
    PlacedClean[L, T1, T1, U1],
    PlacedClean[L, T2, T2, U2])
  : PlacedClean[L, T[T0, T1, T2], T_, T[U0, U1, U2]] = erased

sealed trait PlacedCleanHigherKind4 extends PlacedCleanHigherKind3:
  given higherKind4[L, T[_, _, _, _], T_, T0, T1, T2, T3, U0, U1, U2, U3](using
    T[T0, T1, T2, T3] =:= T_,
    PlacedClean[L, T0, T0, U0],
    PlacedClean[L, T1, T1, U1],
    PlacedClean[L, T2, T2, U2],
    PlacedClean[L, T3, T3, U3])
  : PlacedClean[L, T[T0, T1, T2, T3], T_, T[U0, U1, U2, U3]] = erased

sealed trait PlacedCleanHigherKind5 extends PlacedCleanHigherKind4:
  given higherKind5[L, T[_, _, _, _, _], T_, T0, T1, T2, T3, T4, U0, U1, U2, U3, U4](using
    T[T0, T1, T2, T3, T4] =:= T_,
    PlacedClean[L, T0, T0, U0],
    PlacedClean[L, T1, T1, U1],
    PlacedClean[L, T2, T2, U2],
    PlacedClean[L, T3, T3, U3],
    PlacedClean[L, T4, T4, U4])
  : PlacedClean[L, T[T0, T1, T2, T3, T4], T_, T[U0, U1, U2, U3, U4]] = erased

sealed trait PlacedCleanHigherKind6 extends PlacedCleanHigherKind5:
  given higherKind6[L, T[_, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, U0, U1, U2, U3, U4, U5](using
    T[T0, T1, T2, T3, T4, T5] =:= T_,
    PlacedClean[L, T0, T0, U0],
    PlacedClean[L, T1, T1, U1],
    PlacedClean[L, T2, T2, U2],
    PlacedClean[L, T3, T3, U3],
    PlacedClean[L, T4, T4, U4],
    PlacedClean[L, T5, T5, U5])
  : PlacedClean[L, T[T0, T1, T2, T3, T4, T5], T_, T[U0, U1, U2, U3, U4, U5]] = erased

sealed trait PlacedCleanHigherKind7 extends PlacedCleanHigherKind6:
  given higherKind7[L, T[_, _, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, T6, U0, U1, U2, U3, U4, U5, U6](using
    T[T0, T1, T2, T3, T4, T5, T6] =:= T_,
    PlacedClean[L, T0, T0, U0],
    PlacedClean[L, T1, T1, U1],
    PlacedClean[L, T2, T2, U2],
    PlacedClean[L, T3, T3, U3],
    PlacedClean[L, T4, T4, U4],
    PlacedClean[L, T5, T5, U5],
    PlacedClean[L, T6, T6, U6])
  : PlacedClean[L, T[T0, T1, T2, T3, T4, T5, T6], T_, T[U0, U1, U2, U3, U4, U5, U6]] = erased

sealed trait PlacedCleanHigherKind8 extends PlacedCleanHigherKind7:
  given higherKind8[L, T[_, _, _, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, T6, T7, U0, U1, U2, U3, U4, U5, U6, U7](using
    T[T0, T1, T2, T3, T4, T5, T6, T7] =:= T_,
    PlacedClean[L, T0, T0, U0],
    PlacedClean[L, T1, T1, U1],
    PlacedClean[L, T2, T2, U2],
    PlacedClean[L, T3, T3, U3],
    PlacedClean[L, T4, T4, U4],
    PlacedClean[L, T5, T5, U5],
    PlacedClean[L, T6, T6, U6],
    PlacedClean[L, T7, T7, U7])
  : PlacedClean[L, T[T0, T1, T2, T3, T4, T5, T6, T7], T_, T[U0, U1, U2, U3, U4, U5, U6, U7]] = erased

sealed trait PlacedCleanSelectionByType extends PlacedCleanHigherKind8:
  given selection[L, T, P](using scala.DummyImplicit):
    PlacedClean[L, T from P, T from P, Unit] = erased

sealed trait PlacedCleanSelectionByInstance extends PlacedCleanSelectionByType:
  given selectionSingle[L, T, P](using scala.DummyImplicit):
    PlacedClean[L, T fromSingle P, T fromSingle P, Unit] = erased
  given selectionMultiple[L, T, P](using scala.DummyImplicit):
    PlacedClean[L, T fromMultiple P, T fromMultiple P, Unit] = erased

sealed trait PlacedCleanRemotePeer extends PlacedCleanSelectionByInstance:
  given remotePeer0[L, T, P, _on_[T, P] <: T on P](using scala.DummyImplicit)
    : PlacedClean[L, Placement.Context[P] ?=> (T _on_ P), Placement.Context[P] ?=> (T _on_ P), Unit] = erased
  given remotePeer1[L, T, P, _on_[T, P] <: T on P](using scala.DummyImplicit)
    : PlacedClean[L, T _on_ P, T _on_ P, Unit] = erased

sealed trait PlacedCleanLocalPeer extends PlacedCleanRemotePeer:
  given localPeer0[L <: P, T, U, P, _on_[T, P] <: T on P](using PlacedClean[L, T, T, U])
    : PlacedClean[L, Placement.Context[P] ?=> (T _on_ P), Placement.Context[P] ?=> (T _on_ P), U] = erased
  given localPeer1[L <: P, T, U, P, _on_[T, P] <: T on P](using PlacedClean[L, T, T, U])
    : PlacedClean[L, T _on_ P, T _on_ P, U] = erased

sealed trait PlacedCleanLocalPeerLocal extends PlacedCleanLocalPeer:
  given localPeerLocal0[L, T, U, P, _on_[T, P] <: T on P, _Local_[T] <: Local[T]](using PlacedClean[L, T, T, U])
    : PlacedClean[L, Placement.Context[P] ?=> (_Local_[T] _on_ P), Placement.Context[P] ?=> (_Local_[T] _on_ P), U] = erased
  given localPeerLocal1[L, T, U, P, _on_[T, P] <: T on P, _Local_[T] <: Local[T]](using PlacedClean[L, T, T, U])
    : PlacedClean[L, _Local_[T] _on_ P, _Local_[T] _on_ P, U] = erased

sealed trait PlacedCleanSubjective extends PlacedCleanLocalPeerLocal:
  given subjective0[L, T, P, R, _on_[T, P] <: T on P]
    : PlacedClean[L, Placement.Context[P] ?=> (T per R _on_ P), Placement.Context[P] ?=> (T per R _on_ P), Unit] = erased
  given subjective1[L, T, P, R, _on_[T, P] <: T on P]
    : PlacedClean[L, T per R _on_ P, T per R _on_ P, Unit] = erased

sealed trait PlacedCleanOf extends PlacedCleanSubjective:
  given of[L, T <: Nothing, P, U](using PlacedClean[L, T, T, U])
    : PlacedClean[L, T of P, T of P, U] = erased

sealed trait PlacedCleanAny extends PlacedCleanOf:
  given any[L]: PlacedClean[L, Any, Any, Any] = erased

sealed trait PlacedCleanNull extends PlacedCleanAny:
  given `null`[L]: PlacedClean[L, Null, Null, Null] = erased

sealed trait PlacedCleanNothingSubjective extends PlacedCleanNull:
  given nothing[L]: PlacedClean[L, Nothing, Nothing, Nothing] = erased

sealed trait PlacedCleanAmbiguousResolutionBarrier extends PlacedCleanNothingSubjective:
  given anything[L, T, U](using DummyImplicit.Resolvable): PlacedClean[L, T, T, U] = erased
  given anythingAmbiguous[L, T, U](using DummyImplicit.Resolvable): PlacedClean[L, T, T, U] = erased

object PlacedClean extends PlacedCleanAmbiguousResolutionBarrier:
  transparent inline given clean[L, T](using DummyImplicit.Resolvable): PlacedClean[L, T, T, Nothing] =
    ${ cleanExpr[L, T] }

  def cleanExpr[L: Type, T: Type](using Quotes) = cleanType[L, T] match
    case '[ t ] => '{ erased: PlacedClean[L, T, T, t] } match
      case result: Expr[PlacedClean[L, T, T, Nothing]] @unchecked => result

  def cleanType[L: Type, T: Type](using Quotes) =
    import quotes.reflect.*

    val of = Symbol.requiredPackage("loci.embedding").typeMember("of")
    val on = Symbol.requiredPackage("loci.embedding").typeMember("on")
    val local = Symbol.requiredPackage("loci.language").typeMember("Local")
    val unit = defn.UnitClass.typeRef

    object processor extends TypeMap(quotes):
      override def transform(tpe: TypeRepr) = tpe match
        case _ if tpe.typeSymbol.flags is Flags.Opaque => tpe
        case Refinement(parent, name, _) if name == "on" && parent <:< TypeRepr.of[Nothing] => transform(parent)
        case AppliedType(tycon, List(arg)) if tycon.typeSymbol == of => transform(arg)
        case AppliedType(tycon, List(arg)) if tycon.typeSymbol == local => transform(arg)
        case AppliedType(tycon, List(t, p)) if tycon.typeSymbol == on => t.asType match
          case '[ t `per` r ] => unit
          case _ => if TypeRepr.of[L] <:< p then transform(t) else unit
        case _ => tpe.asType match
          case '[ Nothing ] | '[ Null ] => tpe
          case '[ language.on[t `per` r, p] ] => unit
          case '[ embedding.on[t `per` r, p] ] => unit
          case '[ language.on[t, p] ] => if TypeRepr.of[L] <:< TypeRepr.of[p] then transform(TypeRepr.of[t]) else unit
          case '[ embedding.on[t, p] ] => if TypeRepr.of[L] <:< TypeRepr.of[p] then transform(TypeRepr.of[t]) else unit
          case '[ t `fromMultiple` p ] => unit
          case '[ t `fromSingle` p ] => unit
          case '[ t `from` p ] => unit
          case _ => super.transform(tpe)

    processor.transform(TypeRepr.of[T]).asType
  end cleanType
end PlacedClean
