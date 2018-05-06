package loci
package transmitter
package transmittable

import _root_.rescala.core.Struct
import _root_.rescala.core.Pulse
import _root_.rescala.core.Scheduler
import _root_.rescala.reactives.Signal
import scala.language.higherKinds

protected[transmitter] trait SignalTransmittable {
  implicit def rescalaSignalTransmittable
      [Sig[T, St <: Struct] <: Signal[T, St], T, S, U, St <: Struct](implicit
      scheduler: Scheduler[St],
      transmittable: Transmittable[(T, String, Boolean), S, (U, String, Boolean)],
      serializable: Serializable[S]) = {
    type From = (T, String, Boolean)
    type To = (U, String, Boolean)

    new PushBasedTransmittable[Sig[T, St], From, S, To, scheduler.Signal[U]] {
      final val ignoredValue = null.asInstanceOf[T]
      final val ignoredString = null.asInstanceOf[String]

      def send(value: Sig[T, St], remote: RemoteRef, endpoint: Endpoint[From, To]) = {
        val signal =
          (value
            map { (_, ignoredString, false) }
            recover { case throwable => (ignoredValue, throwable.toString, false) }
            withDefault { (ignoredValue, ignoredString, true) })

        val observer = signal observe endpoint.send

        endpoint.closed notify { _ => observer.remove }

        signal.readValueOnce
      }

      def receive(value: To, remote: RemoteRef, endpoint: Endpoint[From, To]) = {
        val signal = scheduler.transaction() { _ => scheduler.Var.empty[U] }

        def update(signal: scheduler.Var[U], value: To) = value match {
          case (value, `ignoredString`, false) =>
            signal set value
          case (_, message, false) =>
            scheduler.transaction(signal) { implicit turn =>
              signal admitPulse Pulse.Exceptional(
                new rescala.RemoteReactiveFailure(message))
            }
          case _  =>
            signal.setEmpty
        }

        endpoint.closed notify { _ =>
          signal.setEmpty
          signal.disconnect
        }

        update(signal, value)
        endpoint.receive notify { update(signal, _) }

        signal
      }
    }
  }
}
