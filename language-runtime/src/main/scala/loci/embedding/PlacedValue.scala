package loci
package embedding

import language._

import scala.annotation.unchecked.uncheckedVariance
import scala.language.experimental.macros
import scala.language.dynamics
import scala.language.implicitConversions


abstract class PlacedValue[+T, -P] private[loci] extends Dynamic {
  def selectDynamic(key: String): Unit = macro AccessorResolutionFailure.selectDynamic
  def updateDynamic(key: String)(value: Any): Unit = macro AccessorResolutionFailure.updateDynamic
  def applyDynamic(key: String)(args: Any*): Unit = macro AccessorResolutionFailure.applyDynamic
  def applyDynamicNamed(key: String)(args: Any*): Unit = macro AccessorResolutionFailure.applyDynamic
}

object PlacedValue extends transmitter.RemoteAccessor.Default

sealed trait Placed[+T, -P] extends PlacedValue[T, P] {
  def and[T0, T1, P0, P1, T0_on_P0](v: T0_on_P0)(implicit
    ev0: T0_on_P0 <:< (T0 on P0),
    ev1: CommonSuperType[T, T0, T1],
    ev2: CommonSuperType[P @uncheckedVariance, P0, P1]): T1 on P1
  def to[R, U](r: RemoteSbj[R, T, U]): U
  def from[R]: T @uncheckedVariance from R
  def from[R](r: Remote[R]): T @uncheckedVariance fromSingle R
  def from[R](r: Seq[Remote[R]]): T @uncheckedVariance fromMultiple R
  def from[R, placed[_, _]](r: RemoteSelection[R, placed]): T @uncheckedVariance placed R
}

object Placed {
  implicit def lift[T, U, P](v: T): U on P = erased
  implicit def lift[T, U, P, R](v: Remote[R] => T): U per R on P = erased

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


sealed trait CommonSuperType[-T, -U, R]

sealed trait CommonSuperTypeFallback {
  implicit def fallback[T, U]: CommonSuperType[T, T, U] = erased
}

sealed trait CommonSuperTypeDefault extends CommonSuperTypeFallback {
  implicit def default[T]: CommonSuperType[T, T, T] = erased
}

object CommonSuperType extends CommonSuperTypeDefault {
  implicit def local[T, _Local_[T] <: Local[T]]: CommonSuperType[_Local_[T], _Local_[T], _Local_[T]] = erased
}
