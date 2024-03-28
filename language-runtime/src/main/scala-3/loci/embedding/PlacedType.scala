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


sealed trait PlacedClean[T, T_, U]

sealed trait PlacedCleanDefault:
  given default[T]: PlacedClean[T, T, T] = erased

sealed trait PlacedCleanHigherKind1 extends PlacedCleanDefault:
  given higherKind1[T[_], T_, T0, U0](using
    T[T0] =:= T_,
    PlacedClean[T0, T0, U0])
  : PlacedClean[T[T0], T_, T[U0]] = erased

sealed trait PlacedCleanHigherKind2 extends PlacedCleanHigherKind1:
  given higherKind2[T[_, _], T_, T0, T1, U0, U1](using
    T[T0, T1] =:= T_,
    PlacedClean[T0, T0, U0],
    PlacedClean[T1, T1, U1])
  : PlacedClean[T[T0, T1], T_, T[U0, U1]] = erased

sealed trait PlacedCleanHigherKind3 extends PlacedCleanHigherKind2:
  given higherKind3[T[_, _, _], T_, T0, T1, T2, U0, U1, U2](using
    T[T0, T1, T2] =:= T_,
    PlacedClean[T0, T0, U0],
    PlacedClean[T1, T1, U1],
    PlacedClean[T2, T2, U2])
  : PlacedClean[T[T0, T1, T2], T_, T[U0, U1, U2]] = erased

sealed trait PlacedCleanHigherKind4 extends PlacedCleanHigherKind3:
  given higherKind4[T[_, _, _, _], T_, T0, T1, T2, T3, U0, U1, U2, U3](using
    T[T0, T1, T2, T3] =:= T_,
    PlacedClean[T0, T0, U0],
    PlacedClean[T1, T1, U1],
    PlacedClean[T2, T2, U2],
    PlacedClean[T3, T3, U3])
  : PlacedClean[T[T0, T1, T2, T3], T_, T[U0, U1, U2, U3]] = erased

sealed trait PlacedCleanHigherKind5 extends PlacedCleanHigherKind4:
  given higherKind5[T[_, _, _, _, _], T_, T0, T1, T2, T3, T4, U0, U1, U2, U3, U4](using
    T[T0, T1, T2, T3, T4] =:= T_,
    PlacedClean[T0, T0, U0],
    PlacedClean[T1, T1, U1],
    PlacedClean[T2, T2, U2],
    PlacedClean[T3, T3, U3],
    PlacedClean[T4, T4, U4])
  : PlacedClean[T[T0, T1, T2, T3, T4], T_, T[U0, U1, U2, U3, U4]] = erased

sealed trait PlacedCleanHigherKind6 extends PlacedCleanHigherKind5:
  given higherKind6[T[_, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, U0, U1, U2, U3, U4, U5](using
    T[T0, T1, T2, T3, T4, T5] =:= T_,
    PlacedClean[T0, T0, U0],
    PlacedClean[T1, T1, U1],
    PlacedClean[T2, T2, U2],
    PlacedClean[T3, T3, U3],
    PlacedClean[T4, T4, U4],
    PlacedClean[T5, T5, U5])
  : PlacedClean[T[T0, T1, T2, T3, T4, T5], T_, T[U0, U1, U2, U3, U4, U5]] = erased

sealed trait PlacedCleanHigherKind7 extends PlacedCleanHigherKind6:
  given higherKind7[T[_, _, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, T6, U0, U1, U2, U3, U4, U5, U6](using
    T[T0, T1, T2, T3, T4, T5, T6] =:= T_,
    PlacedClean[T0, T0, U0],
    PlacedClean[T1, T1, U1],
    PlacedClean[T2, T2, U2],
    PlacedClean[T3, T3, U3],
    PlacedClean[T4, T4, U4],
    PlacedClean[T5, T5, U5],
    PlacedClean[T6, T6, U6])
  : PlacedClean[T[T0, T1, T2, T3, T4, T5, T6], T_, T[U0, U1, U2, U3, U4, U5, U6]] = erased

sealed trait PlacedCleanHigherKind8 extends PlacedCleanHigherKind7:
  given higherKind8[T[_, _, _, _, _, _, _, _], T_, T0, T1, T2, T3, T4, T5, T6, T7, U0, U1, U2, U3, U4, U5, U6, U7](using
    T[T0, T1, T2, T3, T4, T5, T6, T7] =:= T_,
    PlacedClean[T0, T0, U0],
    PlacedClean[T1, T1, U1],
    PlacedClean[T2, T2, U2],
    PlacedClean[T3, T3, U3],
    PlacedClean[T4, T4, U4],
    PlacedClean[T5, T5, U5],
    PlacedClean[T6, T6, U6],
    PlacedClean[T7, T7, U7])
  : PlacedClean[T[T0, T1, T2, T3, T4, T5, T6, T7], T_, T[U0, U1, U2, U3, U4, U5, U6, U7]] = erased

sealed trait PlacedCleanSelectionByType extends PlacedCleanHigherKind8:
  given selection[T, P](using scala.DummyImplicit):
    PlacedClean[T from P, T from P, Unit] = erased

sealed trait PlacedCleanSelectionByInstance extends PlacedCleanSelectionByType:
  given selectionSingle[T, P](using scala.DummyImplicit):
    PlacedClean[T fromSingle P, T fromSingle P, Unit] = erased
  given selectionMultiple[L, T, P](using scala.DummyImplicit):
    PlacedClean[T fromMultiple P, T fromMultiple P, Unit] = erased

sealed trait PlacedCleanPlacedValue extends PlacedCleanSelectionByInstance:
  given placed0[T, U, P, _on_[T, P] <: T on P](using PlacedClean[T, T, U])
    : PlacedClean[Placement.Context[P] ?=> (T _on_ P), Placement.Context[P] ?=> (T _on_ P), U] = erased
  given placed1[T, U, P, _on_[T, P] <: T on P](using PlacedClean[T, T, U])
    : PlacedClean[T _on_ P, T _on_ P, U] = erased

sealed trait PlacedCleanLocalPlacedValue extends PlacedCleanPlacedValue:
  given localPlaced0[T, U, P, _on_[T, P] <: T on P, _Local_[T] <: Local[T]](using PlacedClean[T, T, U])
    : PlacedClean[Placement.Context[P] ?=> (_Local_[T] _on_ P), Placement.Context[P] ?=> (_Local_[T] _on_ P), U] = erased
  given localPlaced1[T, U, P, _on_[T, P] <: T on P, _Local_[T] <: Local[T]](using PlacedClean[T, T, U])
    : PlacedClean[_Local_[T] _on_ P, _Local_[T] _on_ P, U] = erased

sealed trait PlacedCleanSubjectivePlacedValue extends PlacedCleanLocalPlacedValue:
  given subjective0[T, P, R, _on_[T, P] <: T on P]
    : PlacedClean[Placement.Context[P] ?=> (T per R _on_ P), Placement.Context[P] ?=> (T per R _on_ P), Unit] = erased
  given subjective1[T, P, R, _on_[T, P] <: T on P]
    : PlacedClean[T per R _on_ P, T per R _on_ P, Unit] = erased

sealed trait PlacedCleanOf extends PlacedCleanSubjectivePlacedValue:
  given of[T <: Nothing, P, U](using PlacedClean[T, T, U])
    : PlacedClean[T of P, T of P, U] = erased

sealed trait PlacedCleanAny extends PlacedCleanOf:
  given any: PlacedClean[Any, Any, Any] = erased

sealed trait PlacedCleanNull extends PlacedCleanAny:
  given `null`: PlacedClean[Null, Null, Null] = erased

sealed trait PlacedCleanNothingSubjective extends PlacedCleanNull:
  given nothing: PlacedClean[Nothing, Nothing, Nothing] = erased

sealed trait PlacedCleanAmbiguousResolutionBarrier extends PlacedCleanNothingSubjective:
  given anything[T, U](using DummyImplicit.Resolvable): PlacedClean[T, T, U] = erased
  given anythingAmbiguous[T, U](using DummyImplicit.Resolvable): PlacedClean[T, T, U] = erased

object PlacedClean extends PlacedCleanAmbiguousResolutionBarrier:
  transparent inline given clean[T](using DummyImplicit.Resolvable): PlacedClean[T, T, Nothing] =
    ${ cleanExpr[T] }

  def cleanExpr[T: Type](using Quotes) = cleanType[T] match
    case '[ t ] => '{ erased: PlacedClean[T, T, t] } match
      case result: Expr[PlacedClean[T, T, Nothing]] @unchecked => result

  def cleanType[T: Type](using Quotes) =
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
          case _ => transform(t)
        case _ => tpe.asType match
          case '[ Nothing ] | '[ Null ] => tpe
          case '[ language.on[t `per` r, p] ] => unit
          case '[ embedding.on[t `per` r, p] ] => unit
          case '[ language.on[t, p] ] => transform(TypeRepr.of[t])
          case '[ embedding.on[t, p] ] => transform(TypeRepr.of[t])
          case '[ t `fromMultiple` p ] => unit
          case '[ t `fromSingle` p ] => unit
          case '[ t `from` p ] => unit
          case _ => super.transform(tpe)

    processor.transform(TypeRepr.of[T]).asType
  end cleanType
end PlacedClean
