package retier

import transmission._
import rescala.events.Event
import rescala.events.ImperativeEvent
import scala.language.higherKinds

protected[retier] trait EventTransmittable {
  implicit def rescalaEventTransmittable[Evt[T] <: Event[T], T, S, U](implicit
      transmittable: Transmittable[T, S, U],
      serializable: Serializable[S]) =
    new PushBasedTransmittable[Evt[T], T, S, U, Event[U]] {
      def send(value: Evt[T], remote: RemoteRef, sending: Sending[T]) = {
        value += sending.send
        null.asInstanceOf[T]
      }

      def receive(value: U, remote: RemoteRef, receiving: Receiving[U]) = {
        val event = new ImperativeEvent[U]
        receiving.receive += event.apply
        event
      }
    }
}
