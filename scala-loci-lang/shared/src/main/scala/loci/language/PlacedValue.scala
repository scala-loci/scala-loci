package loci
package language

import scala.annotation.unchecked.uncheckedVariance
import scala.language.{higherKinds, implicitConversions}


abstract class PlacedValue[+T, -P] private[loci]

sealed trait Placed[+T, -P] extends PlacedValue[T, P] {
  def to[R, U](r: RemoteSbj[R, T, U]): U
  def from[R]: T @uncheckedVariance from R
  def from[R](r: Remote[R]): T @uncheckedVariance fromSingle R
  def from[R](r0: Remote[R], r1: Remote[R], rn: Remote[R]*): T @uncheckedVariance fromMultiple R
}

object Placed {
  sealed trait Subjective[+T, -P]

  object Selected {
    type Single[T]
    type Multiple[T]
  }

  object Selection {
    type Single[T, P] = PlacedValue[Selected.Single[T], P]
    type Multiple[T, P] = PlacedValue[Selected.Multiple[T], P]
  }
}

sealed trait RemoteSbj[R, -T, U]

object RemoteSbj {
  implicit def remote[R, T, U](r: Remote[R])(implicit ev: Subjectivity[T, U]): RemoteSbj[R, T, U] = erased(ev)
}
