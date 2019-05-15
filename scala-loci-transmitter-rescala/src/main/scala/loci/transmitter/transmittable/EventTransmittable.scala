package loci
package transmitter
package transmittable

import _root_.rescala.core.{Pulse, Scheduler, Struct}
import _root_.rescala.reactives.{Event, Signals}
import loci.contexts.Immediate.Implicits.global

import scala.language.higherKinds

protected[transmitter] trait EventTransmittable {
  implicit def rescalaEventTransmittable
      [Evt[T, St <: Struct] <: Event[T, St], T, I, U, St <: Struct](implicit
      scheduler: Scheduler[St],
      transmittable: Transmittable[(T, String), I, (U, String)])
  : ConnectedTransmittable.Proxy[Evt[T, St], I, scheduler.Event[U]] {
      type Proxy = scheduler.Event[U]
      type Internal = scheduler.Evt[U]
      type Message = transmittable.Type
  } = {
    val ignoredValue = null.asInstanceOf[T]
    val ignoredString = null.asInstanceOf[String]

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
      },

      direct = (event, context) => event,

      proxy = (future, context) => Signals.fromFuture(future).flatten)
  }
}
