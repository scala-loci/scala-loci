package loci
package language

import scala.annotation.implicitNotFound

object Placement {
  @implicitNotFound("Expression must be placed on a peer")
  sealed trait Context[P]

  sealed trait On[P] {
    def apply[T, U](v: Context[P] => T)(implicit ev: PlacedClean[U on P, P, T, T, U]): U on P
    def local[T, U](v: Context[P] => T)(implicit ev: PlacedClean[U on P, P, T, T, U]): Local[U] on P
    def sbj[R, T, U](v: Context[P] => Remote[R] => T)(implicit ev: PlacedClean[U on P, P, T, T, U]): U per R on P
  }

  sealed trait Placed {
    def apply[P, T, U, S](v: Context[P] => T)(implicit ev0: PlacedType[T, S], ev1: PlacedClean[U on P, P, S, S, U]): U on P
  }

  sealed trait Select[Command[_, _[_, _]]] {
    def apply[P, Q, _on_[T, P] <: T on P](r: Remote[P] _on_ Q): Command[P, fromSingle]
    def apply[P](r: Remote[P]): Command[P, fromSingle]
    def apply[P](r0: Remote[P], r1: Remote[P], rn: Remote[P]*): Command[P, fromMultiple]
    def apply[P](r: Seq[Remote[P]]): Command[P, fromMultiple]
  }

  sealed trait SelectAny[Command[_, _[_, _]]] {
    def apply[P](rule: => Remote[P]): Command[P, fromSingle]
    def apply[P](implicit rule: Seq[Remote[P]] => Remote[P]): Command[P, fromSingle]
    def apply[P](rule: (Seq[Remote[P]], SelfReference[P]) => Remote[P]): Command[P, fromSingle]
  }

  sealed trait Run[P, placed[_, _]] {
    def run: Capture[P, placed] with Block[P, placed]
  }

  sealed trait Capture[P, placed[_, _]] {
    def capture(v: Any*): Block[P, placed]
  }

  sealed trait Block[P, placed[_, _]] {
    def apply[T, U, U_placed_P](v: Context[P] => T)(implicit
      ev0: PlacedClean[U on P, P, T, T, U],
      ev1: CanonicalPlacedTypeAlias[U placed P, U_placed_P]): U_placed_P
    def sbj[R, T, U, U_per_R_placed_P](v: Context[P] => Remote[R] => T)(implicit
      ev0: PlacedClean[U on P, P, T, T, U],
      ev1: CanonicalPlacedTypeAlias[U per R placed P, U_per_R_placed_P]): U_per_R_placed_P
  }

  sealed trait Narrow {
    def apply[P, T, _on_[T, P] <: T on P](v: T _on_ P): T from P
  }

  sealed trait Call[Q, placed[_, _]] {
    def call[P, R, T, _on_[T, P] <: T on P, T_placed_P](v: T _on_ R)(implicit
      ev0: PeerType[Q, R, P],
      ev1: CanonicalPlacedTypeAlias[T placed P, T_placed_P]): T_placed_P
  }
}
