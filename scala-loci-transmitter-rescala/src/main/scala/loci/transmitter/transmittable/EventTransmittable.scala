package loci
package transmitter
package transmittable

import _root_.rescala.core.{Pulse, ReSerializable, Scheduler, Struct}
import _root_.rescala.interface.RescalaInterface
import _root_.rescala.reactives.{Event, Evt, Signals}
import loci.contexts.Immediate.Implicits.global

import scala.language.higherKinds

protected[transmitter] trait EventTransmittable {
  implicit def rescalaEventTransmittable
      [E[T, St <: Struct] <: Event[T, St], T, I, U, St <: Struct](implicit
      scheduler: Scheduler[St],
      transmittable: Transmittable[(Option[T], Option[String]), I, (Option[U], Option[String])])
  : ConnectedTransmittable.Proxy[E[T, St], I, Event[U, St]] {
      type Proxy = Event[U, St]
      type Internal = Evt[U, St]
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
        implicit val serializer: ReSerializable[Evt[U, St]] = ReSerializable.noSerializer
        Signals.fromFuture(future).flatten
      })
  }
}
