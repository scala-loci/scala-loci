package loci
package transmitter
package transmittable

import _root_.rescala.core.{Pulse, ReSerializable, Scheduler, Struct}
import _root_.rescala.interface.RescalaInterface
import _root_.rescala.reactives.{Signal, Signals, Var}
import loci.contexts.Immediate.Implicits.global

protected[transmitter] trait SignalTransmittable {
  implicit def rescalaSignalTransmittable[T, I, U, S <: Struct](implicit
      scheduler: Scheduler[S],
      transmittable: Transmittable[(Option[T], Option[String]), I, (Option[U], Option[String])])
  : ConnectedTransmittable.Proxy[Signal[T, S], I, Signal[U, S]] {
      type Proxy = Signal[U, S]
      type Internal = Var[U, S]
      type Message = transmittable.Type
  } = {
    val interface = RescalaInterface.interfaceFor(scheduler)

    ConnectedTransmittable.Proxy(
      provide = (value, context) => {
        val signal =
          (value
            map { value => Some(value) -> None }
            recover { case exception => None -> Some(RemoteAccessException.serialize(exception)) }
            withDefault { None -> None })

        val observer = signal observe context.endpoint.send

        context.endpoint.closed notify { _ => observer.remove() }

        signal.readValueOnce
      },

      receive = (value, context) => {
        implicit val serializer: ReSerializable[U] = ReSerializable.noSerializer

        val signal = interface.transaction() { _ => interface.Var.empty[U] }

        def update(signal: interface.Var[U], value: (Option[U], Option[String])) =
          value match {
            case (Some(value), _) =>
              signal set value

            case (_, Some(value)) =>
              interface.transaction(signal) { implicit turn =>
                signal.admitPulse(Pulse.Exceptional(RemoteAccessException.deserialize(value)))
              }

            case _  =>
              signal.setEmpty()
          }

        context.endpoint.closed notify { _ =>
          signal.setEmpty()
          signal.disconnect()
        }

        update(signal, value)
        context.endpoint.receive notify { update(signal, _) }

        signal
      },

      direct = (signal, context) => signal,

      proxy = (future, context) => {
        implicit val serializer: ReSerializable[Var[U, S]] = ReSerializable.noSerializer
        Signals.fromFuture(future).flatten
      })
  }
}
