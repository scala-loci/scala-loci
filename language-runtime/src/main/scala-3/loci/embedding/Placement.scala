package loci
package embedding

import loci.language.{on as _, *}

import scala.annotation.{compileTimeOnly, experimental}
import scala.quoted.*

object Placement {
  sealed trait Context[P]

  sealed trait ContextFallback:
    @compileTimeOnly("Expression must be placed on a peer")
    given fallback[P]: Context[P]()

  object Context extends ContextFallback:
    @compileTimeOnly("Expression must be placed on a peer")
    given Context[Any]()

  @experimental
  sealed trait On[P]:
    transparent inline def apply[T, U](inline v: Context[P] ?=> T)(using PlacedClean[U on P, P, T, T, U]): U on P =
      ${ inferrablePlacementContextClosure[U on P]('v) }
    transparent inline infix def local[T, U](inline v: Context[P] ?=> T)(using PlacedClean[U on P, P, T, T, U]): Local[U] on P =
      ${ inferrablePlacementContextClosure[Local[U] on P]('v) }
    transparent inline infix def sbj[R, T, U](inline v: Context[P] ?=> Remote[R] => T)(using PlacedClean[U on P, P, T, T, U]): U per R on P =
      ${ inferrablePlacementContextClosure[U per R on P]('v) }

//  sealed trait Placed {
//    def apply[P, T, U, S](v: Context[P] => T)(implicit ev0: PlacedType[T, S], ev1: PlacedClean[U on P, P, S, S, U]): U on P
//  }
//
//  sealed trait Select[Command[_, _[_, _]]] {
//    def apply[P, Q, _on_[T, P] <: T on P](r: Remote[P] _on_ Q): Command[P, fromSingle]
//    def apply[P](r: Remote[P]): Command[P, fromSingle]
//    def apply[P](r0: Remote[P], r1: Remote[P], rn: Remote[P]*): Command[P, fromMultiple]
//    def apply[P](r: Seq[Remote[P]]): Command[P, fromMultiple]
//  }
//
//  sealed trait Run[P, placed[_, _]] {
//    def run: Capture[P, placed] with Block[P, placed]
//  }
//
//  sealed trait Capture[P, placed[_, _]] {
//    def capture(v: Any*): Block[P, placed]
//  }
//
//  sealed trait Block[P, placed[_, _]] {
//    def apply[T, U, U_placed_P](v: Context[P] => T)(implicit
//      ev0: PlacedClean[U on P, P, T, T, U],
//      ev1: CanonicalPlacedTypeAlias[U placed P, U_placed_P]): U_placed_P
//    infix def sbj[R, T, U, U_per_R_placed_P](v: Context[P] => Remote[R] => T)(implicit
//      ev0: PlacedClean[U on P, P, T, T, U],
//      ev1: CanonicalPlacedTypeAlias[U per R placed P, U_per_R_placed_P]): U_per_R_placed_P
//  }
//
//  sealed trait Narrow {
//    def apply[P, T, _on_[T, P] <: T on P](v: T _on_ P): T from P
//  }
//
//  sealed trait Call[Q, placed[_, _]] {
//    infix def call[P, R, T, _on_[T, P] <: T on P, T_placed_P](v: T _on_ R)(implicit
//      ev0: PeerType[Q, R, P],
//      ev1: CanonicalPlacedTypeAlias[T placed P, T_placed_P]): T_placed_P
//  }
}
