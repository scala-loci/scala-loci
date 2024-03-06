package loci
package language
package transmitter

import loci.embedding._
import loci.transmitter._

import scala.concurrent.Future

abstract class Transmission[V, R, +T, L, M] private[loci] {
  private[loci] def cache[B <: AnyRef](id: Any, body: => B): B
  private[loci] val remoteJoined: Notice.Stream[Remote[R]]
  private[loci] val remoteLeft: Notice.Stream[Remote[R]]
  private[loci] def remotesReferences: Seq[Remote[R]]
  private[loci] def retrieveValues: Seq[T]
}

sealed trait TransmissionNothing {
  implicit def transmissionNothing[L, R, V, W, T, M](implicit
    ev0: Placement.Context.Resolution[L],
    ev1: Multiplicity[L, R, V, W, M],
    ev2: W <:< (Nothing per _),
    ev3: T =:= Future[Nothing]): Transmission[V, R, T, L, M] = erased(ev0, ev1, ev2, ev3)
}

object Transmission extends TransmissionNothing {
  implicit def transmission[L, R, V, W, B, I, P, T, M, U, S <: Transmittables](implicit
    ev0: Placement.Context.Resolution[L],
    ev1: Multiplicity[L, R, V, W, M],
    ev2: Subjectivity[W, B],
    ev3: Transmittable.Resolution[B, I, U, P, S],
    ev4: T =:= P): Transmission[V, R, T, L, M] = erased(ev0, ev1, ev2, ev3, ev4)
}
