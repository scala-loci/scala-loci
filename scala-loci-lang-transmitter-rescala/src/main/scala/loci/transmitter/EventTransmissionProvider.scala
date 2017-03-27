package loci
package transmitter

import contexts.Immediate.Implicits.global
import _root_.rescala.core.Struct
import _root_.rescala.core.Engine
import _root_.rescala.reactives.{ Event => EngineEvent }
import scala.concurrent.Future
import scala.language.higherKinds

protected[transmitter] trait EventTransmissionProvider {
  private final val asLocalId = 0
  private final val asLocalSeqId = 1

  implicit class RescalaEventMultipleTransmissionProvider
      [Evt[T, ES <: Struct] <: EngineEvent[T, ES], T,
       R <: Peer, L <: Peer, ES <: Struct]
      (transmission: MultipleTransmission[Evt[T, ES], R, L])
      (implicit val engine: Engine[ES])
    extends TransmissionProvider {
    import engine.{ Signals, Signal, Var, Event, Evt, transaction }

    private lazy val emptyEvent = Evt[Nothing]

    lazy val asLocalFromAll: Signal[Map[Remote[R], Event[T]]] =
      transmission.memo(asLocalId) {
        val mapping = transaction() { _ => Var(Map.empty[Remote[R], Event[T]]) }

        def insert(remote: Remote[R], futureEvent: Future[Event[T]]) =
          mapping transform {
            _ + (remote -> Signals.fromFuture(futureEvent).flatten)
          }

        def remove(remote: Remote[R]) =
          mapping transform { _ - remote }

        transmission.remoteJoined notify { remote =>
          transmission.retrieveMappedRemoteValues get remote foreach {
            insert(remote, _)
          }
        }

        transmission.remoteLeft notify remove

        transmission.retrieveMappedRemoteValues foreach { (insert _).tupled(_) }

        mapping
      }

    lazy val asLocalFromAllSeq: Event[(Remote[R], T)] = transmission.memo(asLocalSeqId) {
      Signal {
        asLocalFromAll() map { case (remote, event) =>
          event map { (remote, _) }
        } reduceOption {
          _ || _
        } getOrElse emptyEvent
      }.flatten
    }
  }

  implicit class RescalaEventOptionalTransmissionProvider
      [Evt[T, ES <: Struct] <: EngineEvent[T, ES], T,
       R <: Peer, L <: Peer, ES <: Struct]
      (transmission: OptionalTransmission[Evt[T, ES], R, L])
      (implicit val engine: Engine[ES])
    extends TransmissionProvider {
    import engine.{ Signals, Signal, Var, Event, Evt, transaction }

    private lazy val emptyEvent = Evt[Nothing]

    lazy val multiple =
      RescalaEventMultipleTransmissionProvider(transmission.multiple)

    lazy val asLocal: Signal[Option[Event[T]]] = transmission.memo(asLocalId) {
      val option = transaction() { _ => Var(Option.empty[Event[T]]) }

      def update() = option set
        (transmission.retrieveRemoteValue map { Signals.fromFuture(_).flatten })

      transmission.remoteJoined notify { _ => update }
      transmission.remoteLeft notify { _ => update }
      update

      option
    }

    lazy val asLocalSeq: Event[T] = transmission.memo(asLocalSeqId) {
      Signal { asLocal() getOrElse emptyEvent }.flatten
    }
  }

  implicit class RescalaEventSingleTransmissionProvider
      [Evt[T, ES <: Struct] <: EngineEvent[T, ES], T,
       R <: Peer, L <: Peer, ES <: Struct]
      (transmission: SingleTransmission[Evt[T, ES], R, L])
      (implicit val engine: Engine[ES])
    extends TransmissionProvider {
    import engine.{ Signals, Event }

    lazy val optional =
      RescalaEventOptionalTransmissionProvider(transmission.optional)

    lazy val multiple =
      RescalaEventMultipleTransmissionProvider(transmission.multiple)

    lazy val asLocal: Event[T] = transmission.memo(asLocalId) {
      Signals.fromFuture(transmission.retrieveRemoteValue).flatten
    }
  }
}