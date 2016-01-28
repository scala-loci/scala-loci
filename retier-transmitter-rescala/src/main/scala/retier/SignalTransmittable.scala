package retier

import transmission._
import rescala.turns.Engine
import rescala.turns.Turn
import rescala.graph.Spores
import rescala.Signal
import scala.language.higherKinds

protected[retier] trait SignalTransmittable {
  implicit def rescalaSignalTransmittable
      [Sig[T, ES <: Spores] <: Signal[T, ES], T, S, U, ES <: Spores](implicit
      engine: Engine[ES, Turn[ES]],
      transmittable: Transmittable[T, S, U],
      serializable: Serializable[S]) =
    new PushBasedTransmittable[Sig[T, ES], T, S, U, engine.Signal[U]] {
      def send(value: Sig[T, ES], remote: RemoteRef, enpoint: Endpoint[T, U]) = {
        value.changed += enpoint.send
        value.now
      }

      def receive(value: U, remote: RemoteRef, enpoint: Endpoint[T, U]) = {
        val signal = engine.Var(value)
        enpoint.receive += signal.update
        signal
      }
    }
}
