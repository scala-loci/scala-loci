package loci
package embedding

import loci.language.*

abstract class PlacedValue[-P, +T] private[loci]()
//  extends Dynamic:
//  def selectDynamic(key: String): Unit = macro AccessorResolutionFailure.selectDynamic
//  def updateDynamic(key: String)(value: Any): Unit = macro AccessorResolutionFailure.updateDynamic
//  def applyDynamic(key: String)(args: Any*): Unit = macro AccessorResolutionFailure.applyDynamic
//  def applyDynamicNamed(key: String)(args: Any*): Unit = macro AccessorResolutionFailure.applyDynamic

object PlacedValue // extends transmitter.RemoteAccessor.Default

sealed trait Placed[-P, +T] extends PlacedValue[P, T]:
  def test: Int = 0
//  def and[T0, T1, P0, PT, P1, T0_on_P0](v: T0_on_P0)(implicit
//    ev0: T0_on_P0 <:< (T0 on P0),
//    ev1: CommonSuperType[T, T0, T1],
//    ev2: CommonSuperType[P @uncheckedVariance, P0, PT],
//    ev3: AnyUpcast[PT, P1]): T1 on P1
//  def to[R, U](r: RemoteSbj[R, T, U]): U
//  def from[R]: T @uncheckedVariance from R
//  def from[R](r: Remote[R]): T @uncheckedVariance fromSingle R
//  def from[R](r: Seq[Remote[R]]): T @uncheckedVariance fromMultiple R
//  def from[R, placed[_, _]](r: RemoteSelection[R, placed]): T @uncheckedVariance placed R

object Placed:
  type Value[T]

  infix type on[T, P] = Placed[P, T] & T

  given liftLocal[T, P, U](using (T on P) <:< U): Conversion[T, U] with
    transparent inline def apply(v: T) =
      def body: T = v
      erased: U

  given liftSubjective[T, P, R, U] (using (T per R on P) <:< U): Conversion[Remote[R] => T on P, U] with
    transparent inline def apply(v: Remote[R] => T on P) =
      def body: Remote[R] => T on P = v
      erased: U


  sealed trait Subjective[-P, +T]

  object Subjective:
    infix type on[T, P] = Remote[Subjective.R[T]] => Placed.on[Subjective.T[T], P]
    type T[`T per R`] = `T per R` match { case t per ? => t }
    type R[`T per R`] = `T per R` match { case ? per r => r }

  object Selected:
    type Single[T]
    type Multiple[T]

  object Selection:
    type Single[P, T] = PlacedValue[P, Selected.Single[T]]
    type Multiple[P, T] = PlacedValue[P, Selected.Multiple[T]]


//sealed trait RemoteSbj[R, -T, U]
//
//object RemoteSbj {
//  implicit def remote[R, T, U](r: Remote[R])(implicit ev: Subjectivity[T, U]): RemoteSbj[R, T, U] = erased(ev)
//}
//
//
//sealed trait CommonSuperType[-T, -U, R]
//
//sealed trait CommonSuperTypeFallback {
//  implicit def fallback[T, U]: CommonSuperType[T, T, U] = erased
//}
//
//sealed trait CommonSuperTypeDefault extends CommonSuperTypeFallback {
//  implicit def default[T]: CommonSuperType[T, T, T] = erased
//}
//
//object CommonSuperType extends CommonSuperTypeDefault {
//  implicit def local[T, _Local_[T] <: Local[T]]: CommonSuperType[_Local_[T], _Local_[T], _Local_[T]] = erased
//}
//
//
//sealed trait AnyUpcast[T, R]
//
//sealed trait AnyUpcastFallback {
//  implicit def fallback[T, U]: AnyUpcast[T, U] = erased
//}
//
//sealed trait AnyUpcastDefault extends AnyUpcastFallback {
//  implicit def default[T]: AnyUpcast[T, T] = erased
//}
//
//sealed trait AnyUpcastAnyVal extends AnyUpcastDefault {
//  implicit def anyval: AnyUpcast[AnyVal, Any] = erased
//}
//
//sealed trait AnyUpcastAnyRef extends AnyUpcastAnyVal {
//  implicit def anyref: AnyUpcast[AnyRef, Any] = erased
//}
//
//object AnyUpcast extends AnyUpcastAnyRef {
//  implicit def obj: AnyUpcast[Object, Any] = erased
//}