package retier

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scala.language.higherKinds

trait SignalDefaultValues extends SignalDefaultTuples {
  case class SignalDefaultValue[T](value: T)

  object SignalDefaultValue extends
    SignalDefaultSimpleValues with
    SignalDefaultCollectionValues with
    SignalDefaultTupleValues

  case class NoSignalDefaultValue[T]()

  object NoSignalDefaultValue {
    implicit def noDefault[T] = NoSignalDefaultValue[T]
    implicit def noDefaultAmbiguousEvidence[T](
      implicit ev: SignalDefaultValue[T]) = NoSignalDefaultValue[T]
  }

  trait BasicSignalDefaultValues {
    protected def default[T: SignalDefaultValue] =
      implicitly[SignalDefaultValue[T]].value

    // implicit def defaultNull[T >: Null] = SignalDefaultValue[T](null)
  }

  trait SignalDefaultSimpleValues extends BasicSignalDefaultValues {
    implicit object defaultString extends SignalDefaultValue[String]("")
    implicit object defaultDouble extends SignalDefaultValue[Double](0)
    implicit object defaultFloat extends SignalDefaultValue[Float](0)
    implicit object defaultLong extends SignalDefaultValue[Long](0)
    implicit object defaultInt extends SignalDefaultValue[Int](0)
    implicit object defaultShort extends SignalDefaultValue[Short](0)
    implicit object defaultByte extends SignalDefaultValue[Byte](0)
    implicit object defaultChar extends SignalDefaultValue[Char](0)
    implicit object defaultBoolean extends SignalDefaultValue[Boolean](false)
    implicit object defaultUnit extends SignalDefaultValue[Unit](())
  }

  trait SignalDefaultCollectionValues extends BasicSignalDefaultValues {
    implicit def defaultTraversable[T, V[T] <: Traversable[T]](
        implicit cbf: CanBuildFrom[Nothing, T, V[T]]) =
      SignalDefaultValue[V[T]](cbf().result)

    implicit def defaultArray[T: ClassTag] =
      SignalDefaultValue(Array.empty)

    implicit def defaultMap[T] =
      SignalDefaultValue(Map.empty)

    implicit def defaultOption[T] =
      SignalDefaultValue(Option.empty)

    implicit def defaultSome[T: SignalDefaultValue] =
      SignalDefaultValue(Some(default[T]))

    implicit def defaultEitherLeft
      [L: SignalDefaultValue, R] =
      SignalDefaultValue(Left(default[L])): SignalDefaultValue[Either[L, R]]

    implicit def defaultEitherRight
      [L: NoSignalDefaultValue, R: SignalDefaultValue] =
      SignalDefaultValue(Right(default[R])): SignalDefaultValue[Either[L, R]]

    implicit def defaultLeft[L: SignalDefaultValue, R] =
      SignalDefaultValue(Left(default[L]))

    implicit def defaultRight[L, R: SignalDefaultValue] =
      SignalDefaultValue(Right(default[R]))
  }
}
