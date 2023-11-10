package loci.transmitter.internal

import _root_.rescala.interface.RescalaInterface
import _root_.rescala.operator.Pulse
import loci.transmitter.{ConnectedTransmittable, RemoteAccessException, Transmittable}

private[loci] trait RescalaSignalTransmittable {
  val interface: RescalaInterface
  import interface._

  implicit def rescalaSignalTransmittable[T, I, U](implicit
      transmittable: Transmittable[(Option[T], Option[String]), I, (Option[U], Option[String])])
  : ConnectedTransmittable.Proxy[Signal[T], I, Signal[U]] {
      type Proxy = Signal[U]
      type Internal = Var[U]
      type Message = transmittable.Type
  } = {
    ConnectedTransmittable.Proxy(
      internal = Var.empty[U],

      provide = (value, context) => {
        val signal =
          (value
            map { value => Some(value) -> None }
            recover { case exception => None -> Some(RemoteAccessException.serialize(exception)) }
            withDefault { None -> None })

        val observer = signal observe context.endpoint.send

        context.endpoint.closed foreach { _ => observer.disconnect() }

        signal.readValueOnce
      },

      receive = (signal, value, context) => {
        def update(signal: Var[U], value: (Option[U], Option[String])) =
          value match {
            case (Some(value), _) =>
              signal.set(value)

            case (_, Some(value)) =>
              transaction(signal) { implicit ticket =>
                signal.admitPulse(Pulse.Exceptional(RemoteAccessException.deserialize(value)))
              }

            case _  =>
              signal.setEmpty()
          }

        context.endpoint.closed foreach { _ =>
          signal.setEmpty()
          signal.disconnect()
        }

        update(signal, value)
        context.endpoint.receive foreach { update(signal, _) }
      },

      direct = (signal, context) => signal,

      proxy = (signal, completion, context) => {
        completion foreach {
          _.failed foreach { exception =>
            transaction(signal) { implicit ticket =>
              signal.admitPulse(Pulse.Exceptional(exception))
            }
          }
        }

        signal
      })
  }
}
