package retier

import transmission._
import rescala.synchronization.Engines.default
import rescala.synchronization.Engines.default._
import scala.language.higherKinds

protected[retier] trait EventTransmittable {
  implicit def rescalaEventTransmittable[Evnt[T] <: Event[T], T, S, U](implicit
      transmittable: Transmittable[T, S, U],
      serializable: Serializable[S]) =
    new PushBasedTransmittable[Evnt[T], T, S, U, Event[U]] {
      def send(value: Evnt[T], remote: RemoteRef, enpoint: Endpoint[T, U]) = {
        value += enpoint.send
        null.asInstanceOf[T]
      }

      def receive(value: U, remote: RemoteRef, enpoint: Endpoint[T, U]) = {
        val event = Evt[U]
        enpoint.receive += event.apply
        event
      }
    }
}
