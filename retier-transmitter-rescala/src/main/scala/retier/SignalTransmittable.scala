package retier

import transmission._
import rescala.Signal
import rescala.Var
import scala.language.higherKinds

protected[retier] trait SignalTransmittable {
  implicit def rescalaSignalTransmittable[Sig[T] <: Signal[T], T, S, U](implicit
      transmittable: Transmittable[T, S, U],
      serializable: Serializable[S]) =
    new PushBasedTransmittable[Sig[T], T, S, U, Signal[U]] {
      def send(value: Sig[T], remote: RemoteRef, enpoint: Endpoint[T, U]) = {
        value.changed += enpoint.send
        value.get
      }

      def receive(value: U, remote: RemoteRef, enpoint: Endpoint[T, U]) = {
        val signal = Var(value)
        enpoint.receive += signal.update
        signal
      }
    }
}
