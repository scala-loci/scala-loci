package loci
package transmitter

import contexts.Immediate.Implicits.global
import _root_.rescala.core.Struct
import _root_.rescala.core.Engine
import _root_.rescala.reactives.{ Signal => EngineSignal }
import scala.concurrent.Future
import scala.language.higherKinds

protected[transmitter] trait SignalTransmissionProvider {
  private final val asLocalId = 0

  implicit class RescalaSignalMultipleTransmissionProvider
      [Sig[T, ES <: Struct] <: EngineSignal[T, ES], T,
       R <: Peer, L <: Peer, ES <: Struct]
      (transmission: MultipleTransmission[Sig[T, ES], R, L])
      (implicit val engine: Engine[ES])
    extends TransmissionProvider {
    import engine.{ Signals, Signal, Var, transaction }

    lazy val asLocalFromAll: Signal[Map[Remote[R], Signal[T]]] =
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
      [Sig[T, ES <: Struct] <: EngineSignal[T, ES], T,
       R <: Peer, L <: Peer, ES <: Struct]
      (transmission: OptionalTransmission[Sig[T, ES], R, L])
      (implicit val engine: Engine[ES])
    extends TransmissionProvider {
    import engine.{ Signals, Signal, Var, transaction }

    lazy val multiple =
      RescalaSignalMultipleTransmissionProvider(transmission.multiple)

    lazy val asLocal: Signal[Option[T]] = transmission.memo(asLocalId) {
      val option = transaction() { _ => Var(Option.empty[Signal[T]]) }

      def update() = option set (transmission.retrieveRemoteValue map {
        Signals.fromFuture(_).flatten
      })

      transmission.remoteJoined notify { _ => update }
      transmission.remoteLeft notify { _ => update }
      update

      Signal { option() map { _() } }
    }
  }

  implicit class RescalaSignalSingleTransmissionProvider
      [Sig[T, ES <: Struct] <: EngineSignal[T, ES], T,
       R <: Peer, L <: Peer, ES <: Struct]
      (transmission: SingleTransmission[Sig[T, ES], R, L])
      (implicit val engine: Engine[ES])
    extends TransmissionProvider {
    import engine.{ Signals, Signal }

    lazy val optional =
      RescalaSignalOptionalTransmissionProvider(transmission.optional)

    lazy val multiple =
      RescalaSignalMultipleTransmissionProvider(transmission.multiple)

    lazy val asLocal: Signal[T] =  transmission.memo(asLocalId) {
      Signals.fromFuture(transmission.retrieveRemoteValue).flatten
    }
  }
}
