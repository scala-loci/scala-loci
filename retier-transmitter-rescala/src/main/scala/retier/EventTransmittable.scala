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
      def send(value: Evt[T], remote: RemoteRef, enpoint: Endpoint[T, U]) = {
        value += enpoint.send
        null.asInstanceOf[T]
      }

      def receive(value: U, remote: RemoteRef, enpoint: Endpoint[T, U]) = {
        val event = new ImperativeEvent[U]
        enpoint.receive += event.apply
        event
      }
    }
}
