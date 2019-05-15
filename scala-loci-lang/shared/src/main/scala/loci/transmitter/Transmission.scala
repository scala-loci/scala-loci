package loci
package transmitter

import loci.language._

abstract class Transmission[V, R, T, L, M] private[loci] {
  private[loci] def cache[B <: AnyRef](id: Any, body: => B): B
  private[loci] val remoteJoined: Notification[Remote[R]]
  private[loci] val remoteLeft: Notification[Remote[R]]
  private[loci] def remotesReferences: Seq[Remote[R]]
  private[loci] def retrieveValues: Seq[T]
}

object Transmission {
  implicit def transmission[L, R, V, W, B, I, P, T, M, U, S <: Transmittables](implicit
    ev0: Placement.Context[L],
    ev1: Multiplicity[L, R, V, W, M],
    ev2: Subjectivity[W, B],
    ev3: Transmittable.Aux[B, I, U, P, S],
    ev4: T =:= P): Transmission[V, R, T, L, M] = erased(ev0, ev1, ev2, ev3, ev4)
}
