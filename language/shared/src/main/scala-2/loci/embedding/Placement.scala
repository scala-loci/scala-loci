package loci
package embedding

import language._

trait On[P] {
  def apply[T, U](v: Placement.Context[P] => T)(implicit ev: PlacedClean[U on P, P, T, T, U]): U on P = erased(ev)
  def local[T, U](v: Placement.Context[P] => T)(implicit ev: PlacedClean[U on P, P, T, T, U]): Local[U] on P = erased(ev)
  def sbj[R, T, U](v: Placement.Context[P] => Remote[R] => T)(implicit ev: PlacedClean[U on P, P, T, T, U]): U per R on P = erased(ev)
}

object On {
  trait Placed {
    def apply[P, T, U, S](v: Placement.Context[P] => T)(implicit ev0: PlacedType[T, S], ev1: PlacedClean[U on P, P, S, S, U]): U on P = erased(ev0, ev1)
  }
}

trait Select[Command[_, _[_, _]]] {
  def apply[P, Q, _on_[T, P] <: T on P](r: Remote[P] _on_ Q): Command[P, fromSingle] = erased(r)
  def apply[P, Disambiguation](r: Remote[P]): Command[P, fromSingle] = erased(r)
  def apply[P, Disambiguation](r0: Remote[P], r1: Remote[P], rn: Remote[P]*): Command[P, fromMultiple] = erased(r0, r1, rn)
  def apply[P, Disambiguation](r: Seq[Remote[P]]): Command[P, fromMultiple] = erased(r)
}

trait Run[P, placed[_, _]] {
  def run: Capture[P, placed] with Block[P, placed] = erased
}

trait Capture[P, placed[_, _]] {
  def capture(v: Any*): Block[P, placed] = erased(v)
}

trait Block[P, placed[_, _]] {
  def apply[T, U, U_placed_P](v: Placement.Context[P] => T)(implicit
    ev0: PlacedClean[U on P, P, T, T, U],
    ev1: CanonicalPlacedTypeAlias[U placed P, U_placed_P]): U_placed_P = erased(ev0, ev1)
  def sbj[R, T, U, U_per_R_placed_P](v: Placement.Context[P] => Remote[R] => T)(implicit
    ev0: PlacedClean[U on P, P, T, T, U],
    ev1: CanonicalPlacedTypeAlias[U per R placed P, U_per_R_placed_P]): U_per_R_placed_P = erased(ev0, ev1)
}

trait Narrow {
  def apply[P, T, _on_[T, P] <: T on P](v: T _on_ P): T from P = erased
}

trait Call[Q, placed[_, _]] {
  def call[P, R, T, _on_[T, P] <: T on P, T_placed_P](v: T _on_ R)(implicit
    ev0: PeerType[Q, R, P],
    ev1: CanonicalPlacedTypeAlias[T placed P, T_placed_P]): T_placed_P = erased(ev0, ev1)
}
