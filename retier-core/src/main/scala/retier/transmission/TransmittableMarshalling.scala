package retier
package transmission

import scala.util.DynamicVariable
import scala.util.Try

object TransmittableMarshalling {
  private val currentAbstraction =
    new DynamicVariable(Option.empty[(AbstractionRef, Int)])

  type Marshall[T] = T => String

  type Unmarshall[T] = String => Try[T]

  def defaultTransmittable[T] = new PullBasedTransmittable[T, T] {
    def send(value: T) = value
    def receive(value: T) = value
  }

  implicit class PullBasedTransmittableMarshalling[T, U](
      transmittable: PullBasedTransmittable[T, U]) {
    def createMarshallable(
        marshall: Marshall[U], unmarshall: Unmarshall[U]) = {
      def _marshall = marshall
      def _unmarshall = unmarshall

      new PullBasedMarshallable[T] {
        def marshall(unmarshalled: T, abstraction: AbstractionRef) =
          currentAbstraction.withValue(Some((abstraction, 0))) {
            _marshall(transmittable send unmarshalled)
          }
        def unmarshall(marshalled: String, abstraction: AbstractionRef) =
          currentAbstraction.withValue(Some((abstraction, 0))) {
            _unmarshall(marshalled) map { transmittable receive _ }
          }
      }
    }
  }

  implicit class PushBasedTransmittableMarshalling[T, U](
      transmittable: PushBasedTransmittable[T, U]) {
    def createMarshallable(
        marshall: Marshall[U], unmarshall: Unmarshall[U]) = {
      def _marshall = marshall
      def _unmarshall = unmarshall

      new PushBasedMarshallable[T] {
        def marshall(unmarshalled: T, abstraction: AbstractionRef) =
          currentAbstraction.withValue(Some((abstraction, 0))) {
            _marshall(transmittable send (unmarshalled, ???, ???))
          }
        def unmarshall(marshalled: String, abstraction: AbstractionRef) =
          currentAbstraction.withValue(Some((abstraction, 0))) {
            _unmarshall(marshalled) map { transmittable receive (_, ???, ???) }
          }
      }
    }
  }

  implicit class TransmittableMarshalling[T, U](
      transmittable: Transmittable[T, U]) {
    def send(value: => T, marshall: Marshall[U]): U =
      currentAbstraction.value match {
        case Some((abstraction, index)) =>
          transmittable match {
            case transmittable: PullBasedTransmittable[T, U] =>
              transmittable send value

            case transmittable: PushBasedTransmittable[T, U] =>
              val update = 0
              val derived =
                abstraction derive index.toString derive update.toString

              currentAbstraction.value = Some((abstraction, index + 1))
              transmittable send (value, ???, ???)
          }

        case _ =>
          throw new InvalidDynamicScope
      }

    def receive(value: => U, unmarshall: Unmarshall[U]): T =
      currentAbstraction.value match {
        case Some((abstraction, index)) =>
          transmittable match {
            case transmittable: PullBasedTransmittable[T, U] =>
              transmittable receive value

            case transmittable: PushBasedTransmittable[T, U] =>
              val update = 0
              val derived =
                abstraction derive index.toString derive update.toString

              currentAbstraction.value = Some((abstraction, index + 1))
              transmittable receive (value, ???, ???)
          }

        case _ =>
          throw new InvalidDynamicScope
      }
  }

  class InvalidDynamicScope extends RuntimeException(
    "Sending or receiving value on transmittable object " +
    "outside of a related marshallable object dynamic scope")
}
