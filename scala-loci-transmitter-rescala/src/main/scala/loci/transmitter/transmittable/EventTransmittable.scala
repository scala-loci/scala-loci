package loci
package transmitter
package transmittable

import _root_.rescala.interface.RescalaInterface
import _root_.rescala.operator.Pulse

protected[transmitter] trait EventTransmittable {
  val interface: RescalaInterface
  import interface._

  implicit def rescalaEventTransmittable[T, I, U](implicit
      transmittable: Transmittable[(Option[T], Option[String]), I, (Option[U], Option[String])])
  : ConnectedTransmittable.Proxy[Event[T], I, Event[U]] {
      type Proxy = Event[U]
      type Internal = Evt[U]
      type Message = transmittable.Type
  } = {
    ConnectedTransmittable.Proxy(
      internal = Evt[U](),

      provide = (value, context) => {
        val observer =
          (value
            map { value => Some(value) -> None }
            recover { case exception => Some(None -> Some(RemoteAccessException.serialize(exception))) }
            observe context.endpoint.send)

        context.endpoint.closed foreach { _ => observer.remove() }

        None -> None
      },

      receive = (event, value, context) => {
        context.endpoint.receive foreach {
          case (Some(value), _) =>
            event.fire(value)

          case (_, Some(value)) =>
            transaction(event) { implicit ticket =>
              event.admitPulse(Pulse.Exceptional(RemoteAccessException.deserialize(value)))
            }

          case _ =>
        }

        context.endpoint.closed foreach { _ => event.disconnect() }
      },

      direct = (event, context) => event,

      proxy = (event, completion, context) => event)
  }
}
