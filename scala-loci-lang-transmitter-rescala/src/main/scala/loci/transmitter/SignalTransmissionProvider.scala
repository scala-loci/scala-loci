package loci
package transmitter

import contexts.Immediate.Implicits.global
import _root_.rescala.core.Struct
import _root_.rescala.core.Scheduler
import _root_.rescala.macros.cutOutInReactiveMacro
import _root_.rescala.reactives.{ Signal => SchedulerSignal }
import scala.concurrent.Future
import scala.language.higherKinds

protected[transmitter] trait SignalTransmissionProvider {
  private final val asLocalId = 0

  implicit class RescalaSignalMultipleTransmissionProvider
      [Sig[T, St <: Struct] <: SchedulerSignal[T, St], T,
       R <: Peer, L <: Peer, St <: Struct]
      (transmission: MultipleTransmission[Sig[T, St], R, L])
      (implicit val scheduler: Scheduler[St])
    extends TransmissionProvider {
    import scheduler.{ Signals, Signal, Var, transaction }

    lazy val asLocal: Signal[Map[Remote[R], Signal[T]]] @cutOutInReactiveMacro =
      transmission.memo(asLocalId) {
        val mapping = transaction() { _ => Var(Map.empty[Remote[R], Signal[T]]) }

        def insert(remote: Remote[R], futureSignal: Future[Signal[T]]) =
          mapping transform {
            _ + (remote -> Signals.fromFuture(futureSignal).flatten)
          }

        def remove(remote: Remote[R]) = mapping transform { _ - remote }

        transmission.remoteJoined notify { remote =>
          transmission.retrieveMappedRemoteValues get remote foreach {
            insert(remote, _)
          }
        }
        transmission.remoteLeft notify remove
        transmission.retrieveMappedRemoteValues foreach { (insert _).tupled(_) }

        mapping
      }
    }

  implicit class RescalaSignalOptionalTransmissionProvider
      [Sig[T, St <: Struct] <: SchedulerSignal[T, St], T,
       R <: Peer, L <: Peer, St <: Struct]
      (transmission: OptionalTransmission[Sig[T, St], R, L])
      (implicit val scheduler: Scheduler[St])
    extends TransmissionProvider {
    import scheduler.{ Signals, Signal, Var, transaction }

    lazy val multiple =
      RescalaSignalMultipleTransmissionProvider(transmission.multiple)

    lazy val asLocal: Signal[Option[T]] @cutOutInReactiveMacro = transmission.memo(asLocalId) {
      val option = transaction() { _ => Var(Option.empty[Signal[T]]) }

      def update() = option set (transmission.retrieveRemoteValue map {
        Signals.fromFuture(_).flatten
      })

      transmission.remoteJoined notify { _ => update }
      transmission.remoteLeft notify { _ => update }
      update

      option.flatten
    }
  }

  implicit class RescalaSignalSingleTransmissionProvider
      [Sig[T, St <: Struct] <: SchedulerSignal[T, St], T,
       R <: Peer, L <: Peer, St <: Struct]
      (transmission: SingleTransmission[Sig[T, St], R, L])
      (implicit val scheduler: Scheduler[St])
    extends TransmissionProvider {
    import scheduler.{ Signals, Signal }

    lazy val optional =
      RescalaSignalOptionalTransmissionProvider(transmission.optional)

    lazy val multiple =
      RescalaSignalMultipleTransmissionProvider(transmission.multiple)

    lazy val asLocal: Signal[T] @cutOutInReactiveMacro =  transmission.memo(asLocalId) {
      Signals.fromFuture(transmission.retrieveRemoteValue).flatten
    }
  }
}
