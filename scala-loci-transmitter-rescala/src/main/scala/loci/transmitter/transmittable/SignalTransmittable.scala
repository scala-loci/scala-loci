package loci
package transmitter
package transmittable

import _root_.rescala.core.{Pulse, ReSerializable, Scheduler, Struct}
import _root_.rescala.interface.RescalaInterface
import _root_.rescala.reactives.{Signal, Signals, Var}
import loci.contexts.Immediate.Implicits.global

import scala.language.higherKinds

protected[transmitter] trait SignalTransmittable {
  implicit def rescalaSignalTransmittable
      [S[T, St <: Struct] <: Signal[T, St], T, I, U, St <: Struct](implicit
      scheduler: Scheduler[St],
      transmittable: Transmittable[(T, String, Boolean), I, (U, String, Boolean)])
  : ConnectedTransmittable.Proxy[S[T, St], I, Signal[U, St]] {
      type Proxy = Signal[U, St]
      type Internal = Var[U, St]
      type Message = transmittable.Type
  } = {
    val ignoredValue = null.asInstanceOf[T]
    val ignoredString = null.asInstanceOf[String]
    val interface = RescalaInterface.interfaceFor(scheduler)

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
        implicit val serializer: ReSerializable[U] = ReSerializable.noSerializer

        val signal = interface.transaction() { _ => interface.Var.empty[U] }

        def update(signal: interface.Var[U], value: (U, String, Boolean)) =
          value match {
            case (value, `ignoredString`, false) =>
              signal set value
            case (_, message, false) =>
              interface.transaction(signal) { implicit turn =>
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

      proxy = (future, context) => {
        implicit val serializer: ReSerializable[Var[U, St]] = ReSerializable.noSerializer
        Signals.fromFuture(future).flatten
      })
  }
}
