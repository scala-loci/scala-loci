package loci
package transmitter
package transmittable

import _root_.rescala.core.{Pulse, Scheduler, Struct}
import _root_.rescala.reactives.{Signal, Signals}
import loci.contexts.Immediate.Implicits.global

import scala.language.higherKinds

protected[transmitter] trait SignalTransmittable {
  implicit def rescalaSignalTransmittable
      [Sig[T, St <: Struct] <: Signal[T, St], T, I, U, St <: Struct](implicit
      scheduler: Scheduler[St],
      transmittable: Transmittable[(T, String, Boolean), I, (U, String, Boolean)])
  : ConnectedTransmittable.Proxy[Sig[T, St], I, scheduler.Signal[U]] {
      type Proxy = scheduler.Signal[U]
      type Internal = scheduler.Var[U]
      type Message = transmittable.Type
  } = {
    val ignoredValue = null.asInstanceOf[T]
    val ignoredString = null.asInstanceOf[String]

    ConnectedTransmittable.Proxy(
      provide = (value, context) => {
        val signal =
          (value
            map { (_, ignoredString, false) }
            recover { case throwable => (ignoredValue, throwable.toString, false) }
            withDefault { (ignoredValue, ignoredString, true) })

        val observer = signal observe context.endpoint.send

        context.endpoint.closed notify { _ => observer.remove }

        signal.readValueOnce
      },

      receive = (value, context) => {
        val signal = scheduler.transaction() { _ => scheduler.Var.empty[U] }

        def update(signal: scheduler.Var[U], value: (U, String, Boolean)) =
          value match {
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

        context.endpoint.closed notify { _ =>
          signal.setEmpty
          signal.disconnect
        }

        update(signal, value)
        context.endpoint.receive notify { update(signal, _) }

        signal
      },

      direct = (signal, context) => signal,

      proxy = (future, context) => Signals.fromFuture(future).flatten)
  }
}
