package retier

import transmission._
import rescala.turns.Engine
import rescala.turns.Turn
import rescala.graph.Spores
import rescala.Event
import scala.language.higherKinds

protected[retier] trait EventTransmittable {
  implicit def rescalaEventTransmittable
      [Evt[T, ES <: Spores] <: Event[T, ES], T, S, U, ES <: Spores](implicit
      engine: Engine[ES, Turn[ES]],
      transmittable: Transmittable[T, S, U],
      serializable: Serializable[S]) =
    new PushBasedTransmittable[Evt[T, ES], T, S, U, engine.Event[U]] {
      def send(value: Evt[T, ES], remote: RemoteRef, enpoint: Endpoint[T, U]) = {
        value += enpoint.send
        null.asInstanceOf[T]
      }

      def receive(value: U, remote: RemoteRef, enpoint: Endpoint[T, U]) = {
        val event = engine.Evt[U]
        enpoint.receive += event.apply
        event
      }
    }
}
