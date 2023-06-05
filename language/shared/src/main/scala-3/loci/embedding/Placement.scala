package loci
package embedding

import loci.language.{on as placedExpression, *}

import scala.annotation.experimental
import scala.quoted.*

object On:
  def apply[P: Type](using Quotes) =
    import quotes.reflect.*
    '{ erased: On[P] & Run[P, from] } match
      case expr: Expr[placedExpression.on[P]] @unchecked => expr

  @experimental
  trait Fallback[P]:
    transparent inline def apply[T, U](
        inline v: Placement.Context[P] ?=> T)(
        using PlacedClean[U on P, P, T, T, U]): U on P =
      ${ impl.inferrableCanonicalPlacementTypeContextClosure[U, U on P]('v) }
    transparent inline infix def local[T, U](
        inline v: Placement.Context[P] ?=> T)(
        using PlacedClean[U on P, P, T, T, U]): Local[U] on P =
      ${ impl.inferrableCanonicalPlacementTypeContextClosure[U, Local[U] on P]('v) }
    transparent inline infix def sbj[R, T, U](
        inline v: Placement.Context[P] ?=> Remote[R] => T)(
        using PlacedClean[U on P, P, T, T, U]): U per R on P =
      ${ impl.inferrableCanonicalPlacementTypeContextClosure[Remote[R] => U, U per R on P]('v) }

@experimental
trait On[P]:
  transparent inline def apply[T](inline v: Placement.Context[P] ?=> T): T on P =
    ${ impl.inferrableCanonicalPlacementTypeContextClosure[T, T on P]('v) }
  transparent inline infix def local[T](inline v: Placement.Context[P] ?=> T): Local[T] on P =
    ${ impl.inferrableCanonicalPlacementTypeContextClosure[T, Local[T] on P]('v) }
  transparent inline infix def sbj[R, T](inline v: Placement.Context[P] ?=> Remote[R] => T): T per R on P =
    ${ impl.inferrableCanonicalPlacementTypeContextClosure[Remote[R] => T, T per R on P]('v) }

trait Select[Command[_, _[_, _]]]:
  def apply[P, Q, _on_[T, P] <: T on P](r: Remote[P] _on_ Q): Command[P, fromSingle] = erased
  def apply[P, Disambiguation](r: Remote[P]): Command[P, fromSingle] = erased
  def apply[P, Disambiguation](r0: Remote[P], r1: Remote[P], rn: Remote[P]*): Command[P, fromMultiple] = erased
  def apply[P, Disambiguation](r: Seq[Remote[P]]): Command[P, fromMultiple] = erased

trait Run[P, placed[_, _]]:
  def run: Capture[P, placed] with Block[P, placed] = erased

trait Capture[P, placed[_, _]]:
  def capture(v: Any*): Block[P, placed] = erased

trait Block[P, placed[_, _]]:
  def apply[T, U, U_placed_P](v: Placement.Context[P] => T)(using
    PlacedClean[U on P, P, T, T, U],
    CanonicalPlacedTypeAlias[U placed P, U_placed_P]): U_placed_P = erased
  infix def sbj[R, T, U, U_per_R_placed_P](v: Placement.Context[P] => Remote[R] => T)(using
    PlacedClean[U on P, P, T, T, U],
    CanonicalPlacedTypeAlias[U per R placed P, U_per_R_placed_P]): U_per_R_placed_P = erased

trait Narrow:
  def apply[P, T, _on_[T, P] <: T on P](v: T _on_ P): T from P = erased

trait Call[Q, placed[_, _]]:
  infix def call[P, R, T, _on_[T, P] <: T on P, T_placed_P](v: T _on_ R)(using
    PeerType[Q, R, P],
    CanonicalPlacedTypeAlias[T placed P, T_placed_P]): T_placed_P = erased
