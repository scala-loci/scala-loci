package loci
package transmitter
package transmittable

import _root_.rescala.core.Struct
import _root_.rescala.core.Pulse
import _root_.rescala.core.Scheduler
import _root_.rescala.reactives.Event
import scala.language.higherKinds

protected[transmitter] trait EventTransmittable {
  implicit def nonNullableTraversable
      [Evt[T, St <: Struct] <: Event[T, St], T, I, U, St <: Struct](implicit
      scheduler: Scheduler[St],
      transmittable: Transmittable[(T, String), I, (U, String)])
  : ConnectedTransmittable[Evt[T, St], I, scheduler.Event[U]] {
      type Message = transmittable.Type
  } = {
    val ignoredValue = null.asInstanceOf[T]
    val ignoredString = null.asInstanceOf[String]

    ConnectedTransmittable(
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
        val event = scheduler.Evt[U]

        context.endpoint.receive notify {
          _ match {
            case (value, `ignoredString`) =>
              event fire value
            case (_, message) =>
              scheduler.transaction(event) { implicit turn =>
                event admitPulse Pulse.Exceptional(
                  new rescala.RemoteReactiveFailure(message))
              }
          }
        }

        context.endpoint.closed notify { _ => event.disconnect }

        event
      })
  }
}
