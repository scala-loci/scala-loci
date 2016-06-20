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

  private class EndpointImpl[T, U](val abstraction: AbstractionRef,
      transmittable: PushBasedTransmittable[_, T, _, U, _])
    extends Endpoint[T, U] {

    private val doSend: T => Unit = withValue(0) { (value, turn) =>
      abstraction.channel send ("Endpoint-Message",
        marshall(value, abstraction derive turn.toString))
      turn + 1
    }

    def send(value: T) = doSend(value)

    private val doReceive = Notifier[U]

    val receive = doReceive.notification

    abstraction.channel.receive +=
      withValue(0) { case ((messageType, payload), turn) =>
        if (messageType == "Endpoint-Message") {
          unmarshall(payload,
            abstraction derive turn.toString) foreach { doReceive(_) }
          turn + 1
        }
        else
          turn
      }

    private def marshall(value: T, abstraction: AbstractionRef) =
      currentAbstraction.withValue(Some((abstraction, 0))) {
        transmittable.marshallable marshal (value, abstraction)
      }

    private def unmarshall(value: String, abstraction: AbstractionRef) =
      currentAbstraction.withValue(Some((abstraction, 0))) {
        transmittable.marshallable unmarshal (value, abstraction)
      }
  }

  private def send[T, S, R](transmittable: Transmittable[T, S, R],
      value: T, abstraction: AbstractionRef): S =
    transmittable match {
      case transmittable: PullBasedTransmittable[_, _, _] =>
        transmittable send (value, abstraction.remote)

      case transmittable: PushBasedTransmittable[_, _, _, _, _] =>
        transmittable.transmittable send (transmittable send (
          value,
          abstraction.remote,
          new EndpointImpl(abstraction, transmittable)))
    }

  private def receive[T, S, R](transmittable: Transmittable[T, S, R],
      value: S, abstraction: AbstractionRef): R =
    transmittable match {
      case transmittable: PullBasedTransmittable[_, _, _] =>
        transmittable receive (value, abstraction.remote)

      case transmittable: PushBasedTransmittable[_, _, _, _, _] =>
        transmittable receive (
          transmittable.transmittable receive value,
          abstraction.remote,
          new EndpointImpl(abstraction, transmittable))
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
    val lock = new Object
    var current = init
    v => lock synchronized { current = f(v, current) }
  }

  class InvalidDynamicScope extends RuntimeException(
    "Sending or receiving value on transmittable object " +
    "outside of a related marshallable object dynamic scope. " +
    "Calls to the `Transmittable` methods `send` and `receive` are only " +
    "allowed directly within the methods `send` and `receive`.")
}
