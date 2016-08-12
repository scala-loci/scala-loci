package retier

import transmission._
import rescala.graph.Struct
import rescala.graph.Pulse
import rescala.engines.Engine
import rescala.propagation.Turn
import rescala.reactives.Event
import scala.util.Success
import scala.util.Failure
import scala.language.higherKinds

protected[retier] trait EventTransmittable {
  implicit def rescalaEventTransmittable
      [Evt[T, ES <: Struct] <: Event[T, ES], T, S, U, ES <: Struct](implicit
      engine: Engine[ES, Turn[ES]],
      transmittable: Transmittable[(T, String), S, (U, String)],
      serializable: Serializable[S]) = {
    type From = (T, String)
    type To = (U, String)

    new PushBasedTransmittable[Evt[T, ES], From, S, To, engine.Event[U]] {
      final val ignoredValue = null.asInstanceOf[T]
      final val ignoredString = null.asInstanceOf[String]

      def send(value: Evt[T, ES], remote: RemoteRef, endpoint: Endpoint[From, To]) = {
        val observer =
          (value
            map { (_, ignoredString) }
            recover { throwable => (ignoredValue, throwable.toString) }
            observe endpoint.send)

        endpoint.closed += { _ => observer.remove }

        null
      }

      def receive(value: To, remote: RemoteRef, endpoint: Endpoint[From, To]) = {
        val event = engine.Evt[U]

        endpoint.receive += {
          case (value, `ignoredString`) =>
            event fire value
          case (_, message) =>
            engine.plan(event) { implicit turn =>
              event admitPulse Pulse.Exceptional(
                new rescalaTransmitter.RemoteReactiveFailure(message))
            }
        }

        endpoint.closed += { _ => event.disconnect }

        event
      }
    }
  }
}
