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
      transmittable: Transmittable[(T, String), I, (U, String)])
  : ConnectedTransmittable.Proxy[E[T, St], I, Event[U, St]] {
      type Proxy = Event[U, St]
      type Internal = Evt[U, St]
      type Message = transmittable.Type
  } = {
    val ignoredValue = null.asInstanceOf[T]
    val ignoredString = null.asInstanceOf[String]
    val interface = RescalaInterface.interfaceFor(scheduler)

    ConnectedTransmittable.Proxy(
      provide = (value, context) => {
        val observer =
          (value
            map { (_, ignoredString) }
            recover { case throwable => Some((ignoredValue, throwable.toString)) }
            observe context.endpoint.send)

        context.endpoint.closed notify { _ => observer.remove }

        null
      },

      receive = (value, context) => {
        val event = interface.Evt[U]

        context.endpoint.receive notify {
          _ match {
            case (value, `ignoredString`) =>
              event fire value
            case (_, message) =>
              interface.transaction(event) { implicit turn =>
                event admitPulse Pulse.Exceptional(
                  new rescala.RemoteReactiveFailure(message))
              }
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
