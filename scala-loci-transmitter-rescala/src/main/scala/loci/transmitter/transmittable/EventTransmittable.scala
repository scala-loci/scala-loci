package loci
package transmitter
package transmittable

import _root_.rescala.core.{Pulse, ReSerializable, Scheduler, Struct}
import _root_.rescala.interface.RescalaInterface
import _root_.rescala.reactives.{Event, Evt, Signals}
import loci.contexts.Immediate.Implicits.global

protected[transmitter] trait EventTransmittable {
  implicit def rescalaEventTransmittable[T, I, U, S <: Struct](implicit
      scheduler: Scheduler[S],
      transmittable: Transmittable[(Option[T], Option[String]), I, (Option[U], Option[String])])
  : ConnectedTransmittable.Proxy[Event[T, S], I, Event[U, S]] {
      type Proxy = Event[U, S]
      type Internal = Evt[U, S]
      type Message = transmittable.Type
  } = {
    val interface = RescalaInterface.interfaceFor(scheduler)

    ConnectedTransmittable.Proxy(
      provide = (value, context) => {
        val observer =
          (value
            map { value => Some(value) -> None }
            recover { case exception => Some(None -> Some(RemoteAccessException.serialize(exception))) }
            observe context.endpoint.send)

        context.endpoint.closed notify { _ => observer.remove() }

        None -> None
      },

      receive = (value, context) => {
        val event = interface.Evt[U]

        context.endpoint.receive notify {
          _ match {
            case (Some(value), _) =>
              event.fire(value)

            case (_, Some(value)) =>
              interface.transaction(event) { implicit turn =>
                event.admitPulse(Pulse.Exceptional(RemoteAccessException.deserialize(value)))
              }

            case _ =>
          }
        }

        context.endpoint.closed notify { _ => event.disconnect }

        event
      },

      direct = (event, context) => event,

      proxy = (future, context) => {
        implicit val serializer: ReSerializable[Evt[U, S]] = ReSerializable.noSerializer
        Signals.fromFuture(future).flatten
      })
  }
}
