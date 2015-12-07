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
      def send(value: Sig[T], remote: RemoteRef, sending: Sending[T]) = {
        value.changed += sending.send
        transmittable send value.get
      }
      def receive(value: S, remote: RemoteRef, receiving: Receiving[U]) = {
        val signal = Var(transmittable receive value)
        receiving.receive += signal.update
        signal
      }
    }
}
