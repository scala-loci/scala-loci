package loci
package transmission

import scala.util.Try
import scala.annotation.implicitNotFound

@implicitNotFound("${T} is not marshallable")
trait Marshallable[T] {
  type Result

  protected final implicit class MarshallableOps[OpsT, OpsS, OpsR](
      marshallable: (Transmittable[OpsT, OpsS, OpsR], Serializable[OpsS]))
    extends transmittableMarshalling.MarshallableOps(marshallable)

  def marshal(unmarshalled: T, abstraction: AbstractionRef): String
  def unmarshal(marshalled: String, abstraction: AbstractionRef): Try[Result]

  def isPushBased: Boolean
}

@implicitNotFound("${T} is not marshallable")
trait MarshallableArgument[T] extends Marshallable[T] {
  type Result = T
}


object Marshallable {
  implicit def marshallable[T, S, R]
    (implicit
        transmittable: Transmittable[T, S, R],
        serializable: Serializable[S]): Marshallable[T] { type Result = R } =
    new Marshallable[T] {
      type Result = R
      def marshal(unmarshalled: T, abstraction: AbstractionRef): String =
        (transmittable, serializable) marshal (unmarshalled, abstraction)
      def unmarshal(marshalled: String, abstraction: AbstractionRef): Try[Result] =
        (transmittable, serializable) unmarshal (marshalled, abstraction)
      def isPushBased = transmittable match {
        case _: PushBasedTransmittable[_, _, _, _, _] => true
        case _: PullBasedTransmittable[_, _, _] => false
      }
    }

  def apply[T](implicit marshallable: Marshallable[T])
    : Marshallable[T] { type Result = marshallable.Result } = marshallable
}

object MarshallableArgument {
  implicit def marshallable[T, S, R]
    (implicit
        transmittable: Transmittable[T, S, R],
        supertype: R <:< T,
        serializable: Serializable[S]): MarshallableArgument[T] =
    new MarshallableArgument[T] {
      def marshal(unmarshalled: T, abstraction: AbstractionRef): String =
        (transmittable, serializable) marshal (unmarshalled, abstraction)
      def unmarshal(marshalled: String, abstraction: AbstractionRef): Try[T] =
        (transmittable, serializable) unmarshal (marshalled, abstraction) map supertype
      def isPushBased = transmittable match {
        case _: PushBasedTransmittable[_, _, _, _, _] => true
        case _: PullBasedTransmittable[_, _, _] => false
      }
    }

  def apply[T](implicit marshallable: MarshallableArgument[T])
    : MarshallableArgument[T] = marshallable
}
