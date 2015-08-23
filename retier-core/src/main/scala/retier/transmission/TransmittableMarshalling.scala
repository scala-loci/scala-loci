package retier
package transmission

import util.Notifier
import scala.util.DynamicVariable
import scala.util.Try

object transmittableMarshalling {
  type Marshall[T] = T => String
  type Unmarshall[T] = String => Try[T]

  def defaultTransmittable[T]: PullBasedTransmittable[T, T] =
    new PullBasedTransmittable[T, T] {
      def send(value: T) = value
      def receive(value: T) = value
    }

  implicit class PullBasedTransmittableMarshalling[T, U](
      transmittable: PullBasedTransmittable[T, U]) {
    def createMarshallable(marshall: Marshall[U], unmarshall: Unmarshall[U])
        : PullBasedMarshallable[T] =
      transmittableMarshalling.createMarshallable(
        transmittable, marshall, unmarshall)
  }

  implicit class PushBasedTransmittableMarshalling[T, U](
      transmittable: PushBasedTransmittable[T, U]) {
    def createMarshallable(marshall: Marshall[U], unmarshall: Unmarshall[U])
        : PushBasedMarshallable[T] =
      transmittableMarshalling.createMarshallable(
        transmittable, marshall, unmarshall)
  }

  implicit class TransmittableMarshalling[T, U](
      transmittable: Transmittable[T, U]) {
    def send(value: => T, marshall: Marshall[U]): U =
      transmittableMarshalling.send(
        transmittable, value, marshall, deriveAbstraction(transmittable))
    def receive(value: => U, unmarshall: Unmarshall[U]): T =
      transmittableMarshalling.receive(
        transmittable, value, unmarshall, deriveAbstraction(transmittable))
  }

  private val currentAbstraction =
    new DynamicVariable(Option.empty[(AbstractionRef, Int)])

  private def createMarshallable[T, U](transmittable: Transmittable[T, U],
      marshall: Marshall[U], unmarshall: Unmarshall[U]) = {
    def _marshall = marshall
    def _unmarshall = unmarshall

    new PullBasedMarshallable[T] with PushBasedMarshallable[T] {
      def marshall(unmarshalled: T, abstraction: AbstractionRef) =
        currentAbstraction.withValue(Some((abstraction, 0))) {
          _marshall(send(transmittable, unmarshalled, _marshall, abstraction))
        }
      def unmarshall(marshalled: String, abstraction: AbstractionRef) =
        currentAbstraction.withValue(Some((abstraction, 0))) {
          _unmarshall(marshalled) map {
            receive(transmittable, _, _unmarshall, abstraction)
          }
        }
    }
  }

  private def send[T, U](transmittable: Transmittable[T, U], value: => T,
      marshall: Marshall[U], abstraction: AbstractionRef): U =
    transmittable match {
      case transmittable: PullBasedTransmittable[T, U] =>
        transmittable send value

      case transmittable: PushBasedTransmittable[T, U] =>
        def _marshall = marshall
        def _abstraction = abstraction

        val sending =
          new { val abstraction = _abstraction } with Sending[U] {
            val doSend: U => Unit = withValue(0) { (value, turn) =>
              abstraction.channel send ("Value-Push",
                marshall(value, abstraction derive turn.toString))
              turn + 1
            }

            def send(value: U) = doSend(value)

            def marshall(value: U, abstraction: AbstractionRef) =
              currentAbstraction.withValue(Some((abstraction, 0))) {
                _marshall(value)
              }
          }

        transmittable send (value, sending)
    }

  private def receive[T, U](transmittable: Transmittable[T, U], value: => U,
      unmarshall: Unmarshall[U], abstraction: AbstractionRef): T =
    transmittable match {
      case transmittable: PullBasedTransmittable[T, U] =>
        transmittable receive value

      case transmittable: PushBasedTransmittable[T, U] =>
        def _unmarshall = unmarshall
        def _abstraction = abstraction

        val receiving =
          new { val abstraction = _abstraction } with Receiving[U] {
            val doReceive = Notifier[U]

            abstraction.channel.receive +=
              withValue(0) { case ((messageType, payload), turn) =>
                if (messageType == "Value-Push") {
                  unmarshall(payload,
                    abstraction derive turn.toString) foreach { doReceive(_) }
                  turn + 1
                }
                else
                  turn
              }

            val receive = doReceive.notification

            def unmarshall(value: String, abstraction: AbstractionRef) =
              currentAbstraction.withValue(Some((abstraction, 0))) {
                _unmarshall(value)
              }
          }

        transmittable receive (value, receiving)
    }

  private def deriveAbstraction(
      transmittable: Transmittable[_, _]): AbstractionRef =
    currentAbstraction.value match {
      case Some((abstraction, index)) =>
        transmittable match {
          case transmittable: PullBasedTransmittable[_, _] =>
            abstraction

          case transmittable: PushBasedTransmittable[_, _] =>
            currentAbstraction.value = Some((abstraction, index + 1))
            abstraction derive index.toString
        }

      case _ =>
        throw new InvalidDynamicScope
    }

  private def withValue[T, U](init: U)(f: (T, U) => U): T => Unit = {
    var current = init
    v => current = f(v, current)
  }

  class InvalidDynamicScope extends RuntimeException(
    "Sending or receiving value on transmittable object " +
    "outside of a related marshallable object dynamic scope")
}
