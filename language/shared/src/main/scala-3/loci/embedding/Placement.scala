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
        using PlacedClean[T, T, U]): U on P =
      ${ impl.inferrableCanonicalPlacementTypeContextClosure[U, U on P]('{ v(using Placement.Context.fallback[P]) }) }
    transparent inline infix def local[T, U](
        inline v: Placement.Context[P] ?=> T)(
        using PlacedClean[T, T, U]): Local[U] on P =
      ${ impl.inferrableCanonicalPlacementTypeContextClosure[U, Local[U] on P]('{ v(using Placement.Context.fallback[P]) }) }
    transparent inline infix def sbj[R, T, U](
        inline v: Placement.Context[P] ?=> Remote[R] => T)(
        using PlacedClean[T, T, U]): U per R on P =
      ${ impl.inferrableCanonicalPlacementTypeContextClosure[Remote[R] => U, U per R on P]('{ v(using Placement.Context.fallback[P]) }) }

  @experimental
  trait Placed:
    transparent inline def apply[T, P](using Placement.Context.ResolutionWithFallback[P])(inline v: Placement.Context[P] ?=> T): T on P =
      ${ impl.inferrableCanonicalPlacementTypeContextClosure[T, T on P]('{ v(using Placement.Context.fallback[P]) }) }

@experimental
trait On[P]:
  transparent inline def apply[T](inline v: Placement.Context[P] ?=> T): T on P =
    ${ impl.inferrableCanonicalPlacementTypeContextClosure[T, T on P]('{ v(using Placement.Context.fallback[P]) }) }
  transparent inline infix def local[T](inline v: Placement.Context[P] ?=> T): Local[T] on P =
    ${ impl.inferrableCanonicalPlacementTypeContextClosure[T, Local[T] on P]('{ v(using Placement.Context.fallback[P]) }) }
  transparent inline infix def sbj[R, T](inline v: Placement.Context[P] ?=> Remote[R] => T): T per R on P =
    ${ impl.inferrableCanonicalPlacementTypeContextClosure[Remote[R] => T, T per R on P]('{ v(using Placement.Context.fallback[P]) }) }

trait Select[Command[_, _[_, _]]]:
  def apply[P, Q, _on_[T, P] <: T on P](r: Remote[P] _on_ Q): Command[P, fromSingle] = erased
  def apply[P, Disambiguation](r: Remote[P]): Command[P, fromSingle] = erased
  def apply[P, Disambiguation](r0: Remote[P], r1: Remote[P], rn: Remote[P]*): Command[P, fromMultiple] = erased
  def apply[P, Disambiguation](r: Seq[Remote[P]]): Command[P, fromMultiple] = erased

trait Run[P, placed[_, _]]:
  def run[R](using Placement.Context.ResolutionWithFallback[R]): Capture[P, R, placed] with Block[P, R, placed] = erased

trait Capture[P, R, placed[_, _]]:
  def capture(v: Any*): Block[P, R, placed] = erased

trait Block[P, R, placed[_, _]]:
  def apply[T, U](v: Placement.Context[P] ?=> T)(using
    PlacedClean[T, T, U]): U placed P = erased
  infix def sbj[R, T, U](v: Placement.Context[P] ?=> Remote[R] => T)(using
    PlacedClean[T, T, U]): U per R placed P = erased

trait Narrow:
  def apply[P, T, _on_[T, P] <: T on P](v: T _on_ P): T from P = erased

trait Call[Q, placed[_, _]]:
  infix def call[P, R, T, _on_[T, P] <: T on P](v: T _on_ R)(using
    PeerType[Q, R, P]): T placed P = erased
