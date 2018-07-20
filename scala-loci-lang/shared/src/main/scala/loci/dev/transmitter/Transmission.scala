package loci.dev
package transmitter

import loci.dev.language._
import loci.transmitter._

abstract class Transmission[V, T, L, M] private[dev] {
  private[dev] def requestValues: Seq[T]
}

object Transmission {
  implicit def transmission[L, R, V, W, B, I, P, T, M, U, S <: Transmittables](implicit
    ev0: Placement.Context[L],
    ev1: Multiplicity[L, R, V, W, M],
    ev2: Subjectivity[W, B],
    ev3: Transmittable.Aux[B, I, U, P, S],
    ev4: T =:= P): Transmission[V from R, T, L, M] = erased(ev0, ev1, ev2, ev3, ev4)
}
