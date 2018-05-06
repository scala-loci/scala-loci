package loci
package transmitter

import contexts.Immediate.Implicits.global
import _root_.rescala.core.Struct
import _root_.rescala.core.Scheduler
import _root_.rescala.macros.cutOutInReactiveMacro
import _root_.rescala.reactives.{ Event => SchedulerEvent }
import scala.concurrent.Future
import scala.language.higherKinds

protected[transmitter] trait EventTransmissionProvider {
  private final val asLocalId = 0
  private final val asLocalSeqId = 1

  implicit class RescalaEventMultipleTransmissionProvider
      [Evt[T, St <: Struct] <: SchedulerEvent[T, St], T,
       R <: Peer, L <: Peer, St <: Struct]
      (transmission: MultipleTransmission[Evt[T, St], R, L])
      (implicit val scheduler: Scheduler[St])
    extends TransmissionProvider {
    import scheduler.{ Signals, Signal, Var, Event, Evt, transaction }

    private lazy val emptyEvent = Evt[Nothing]

    lazy val asLocal: Signal[Map[Remote[R], Event[T]]] @cutOutInReactiveMacro =
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

    lazy val asLocalFromAllSeq: Event[(Remote[R], T)] @cutOutInReactiveMacro = transmission.memo(asLocalSeqId) {
      (asLocal map {
        _ map { case (remote, event) =>
          event map { (remote, _) }
        } reduceOption {
          _ || _
        } getOrElse emptyEvent
      }).flatten
    }
  }

  implicit class RescalaEventOptionalTransmissionProvider
      [Evt[T, St <: Struct] <: SchedulerEvent[T, St], T,
       R <: Peer, L <: Peer, St <: Struct]
      (transmission: OptionalTransmission[Evt[T, St], R, L])
      (implicit val scheduler: Scheduler[St])
    extends TransmissionProvider {
    import scheduler.{ Signals, Signal, Var, Event, Evt, transaction }

    private lazy val emptyEvent = Evt[Nothing]

    lazy val multiple =
      RescalaEventMultipleTransmissionProvider(transmission.multiple)

    lazy val asLocal: Signal[Option[Event[T]]] @cutOutInReactiveMacro = transmission.memo(asLocalId) {
      val option = transaction() { _ => Var(Option.empty[Event[T]]) }

      def update() = option set
        (transmission.retrieveRemoteValue map { Signals.fromFuture(_).flatten })

      transmission.remoteJoined notify { _ => update }
      transmission.remoteLeft notify { _ => update }
      update

      option
    }

    lazy val asLocalSeq: Event[T] @cutOutInReactiveMacro = transmission.memo(asLocalSeqId) {
      (asLocal map { _ getOrElse emptyEvent }).flatten
    }
  }

  implicit class RescalaEventSingleTransmissionProvider
      [Evt[T, St <: Struct] <: SchedulerEvent[T, St], T,
       R <: Peer, L <: Peer, St <: Struct]
      (transmission: SingleTransmission[Evt[T, St], R, L])
      (implicit val scheduler: Scheduler[St])
    extends TransmissionProvider {
    import scheduler.{ Signals, Event }

    lazy val optional =
      RescalaEventOptionalTransmissionProvider(transmission.optional)

    lazy val multiple =
      RescalaEventMultipleTransmissionProvider(transmission.multiple)

    lazy val asLocal: Event[T] @cutOutInReactiveMacro = transmission.memo(asLocalId) {
      Signals.fromFuture(transmission.retrieveRemoteValue).flatten
    }
  }
}
