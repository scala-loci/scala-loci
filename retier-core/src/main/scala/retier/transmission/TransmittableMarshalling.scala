package retier
package transmission

import util.Notifier
import scala.util.DynamicVariable
import scala.util.Try

object transmittableMarshalling {
  private val currentAbstraction =
    new DynamicVariable(Option.empty[(AbstractionRef, Int)])

  implicit class MarshallableOps[T, S, R](
      marshallable: (Transmittable[T, S, R], Serializable[S])) {
    private[this] val (transmittable, serializable) = marshallable

    def marshal(unmarshalled: T, abstraction: AbstractionRef): String =
      currentAbstraction.withValue(Some((abstraction, 0))) {
        serializable serialize send(transmittable, unmarshalled, abstraction)
      }

    def unmarshal(marshalled: String, abstraction: AbstractionRef): Try[R] =
      currentAbstraction.withValue(Some((abstraction, 0))) {
        serializable deserialize marshalled map {
          receive(transmittable, _, abstraction)
        }
      }
  }

  implicit class TransmittableOps[T, S, R](
      transmittable: Transmittable[T, S, R]) {
    def send(value: T): S = transmittableMarshalling.send(
      transmittable, value, deriveAbstraction(transmittable))

    def receive(value: S): R = transmittableMarshalling.receive(
      transmittable, value, deriveAbstraction(transmittable))
  }

  private def send[T, T0, S, R0, R](transmittable: Transmittable[T, S, R],
      value: T, abstraction: AbstractionRef): S =
    transmittable match {
      case transmittable: PullBasedTransmittable[T, S, R] =>
        transmittable send value

      case transmittable: PushBasedTransmittable[T, T0, S, R0, R] @unchecked =>
        def _abstraction = abstraction

        val sending =
          new { val abstraction = _abstraction } with Sending[T0] {
            val doSend: T0 => Unit = withValue(0) { (value, turn) =>
              abstraction.channel send ("Value-Push",
                marshall(value, abstraction derive turn.toString))
              turn + 1
            }

            def send(value: T0) = doSend(value)

            def marshall(value: T0, abstraction: AbstractionRef) =
              currentAbstraction.withValue(Some((abstraction, 0))) {
                transmittable.marshallable marshal (value, abstraction)
              }
          }

        transmittable send (value, sending)
    }

  private def receive[T, T0, S, R0, R](transmittable: Transmittable[T, S, R],
      value: S, abstraction: AbstractionRef): R =
    transmittable match {
      case transmittable: PullBasedTransmittable[T, S, R] =>
        transmittable receive value

      case transmittable: PushBasedTransmittable[T, T0, S, R0, R] @unchecked =>
        def _abstraction = abstraction

        val receiving =
          new { val abstraction = _abstraction } with Receiving[R0] {
            val doReceive = Notifier[R0]

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
                transmittable.marshallable unmarshal (value, abstraction)
              }
          }

        transmittable receive (value, receiving)
    }

  private def deriveAbstraction(
      transmittable: Transmittable[_, _, _]): AbstractionRef =
    currentAbstraction.value match {
      case Some((abstraction, index)) =>
        transmittable match {
          case transmittable: PullBasedTransmittable[_, _, _] =>
            abstraction

          case transmittable: PushBasedTransmittable[_, _, _, _, _] =>
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
