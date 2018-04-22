package loci

import transmission._
import contexts.Immediate.Implicits.global
import rescala.graph.Struct
import rescala.engines.Engine
import rescala.propagation.Turn
import rescala.reactives.Signals
import rescala.reactives.{Signal => EngineSignal}
import scala.concurrent.Future
import scala.language.higherKinds

protected[loci] trait SignalTransmissionProvider {
  private final val asLocalId = 0

  implicit class RescalaSignalMultipleTransmissionProvider
      [Sig[T, ES <: Struct] <: EngineSignal[T, ES], T,
       R <: Peer, L <: Peer, ES <: Struct]
      (transmission: MultipleTransmission[Sig[T, ES], R, L])
      (implicit val engine: Engine[ES, Turn[ES]])
    extends TransmissionProvider {
    import engine._

    lazy val asLocalFromAll: Signal[Map[Remote[R], Signal[T]]] =
      transmission.memo(asLocalId) {
        val mapping = plan() { _ => Var(Map.empty[Remote[R], Signal[T]]) }

        def insert(remote: Remote[R], futureSignal: Future[Signal[T]]) =
          mapping transform {
            _ + (remote -> Signals.fromFuture(futureSignal).flatten)
          }

        def remove(remote: Remote[R]) = mapping transform { _ - remote }

        transmission.remoteJoined += { remote =>
          transmission.retrieveMappedRemoteValues get remote foreach {
            insert(remote, _)
          }
        }
        transmission.remoteLeft += remove
        transmission.retrieveMappedRemoteValues foreach { (insert _).tupled(_) }

        mapping
      }
    }

  implicit class RescalaSignalOptionalTransmissionProvider
      [Sig[T, ES <: Struct] <: EngineSignal[T, ES], T,
       R <: Peer, L <: Peer, ES <: Struct]
      (transmission: OptionalTransmission[Sig[T, ES], R, L])
      (implicit val engine: Engine[ES, Turn[ES]])
    extends TransmissionProvider {
    import engine._

    lazy val multiple =
      RescalaSignalMultipleTransmissionProvider(transmission.multiple)

    lazy val asLocal: Signal[Option[T]] = transmission.memo(asLocalId) {
      val option = plan() { _ => Var(Option.empty[Signal[T]]) }

      def update() = option set (transmission.retrieveRemoteValue map {
        Signals.fromFuture(_).flatten
      })

      transmission.remoteJoined += { _ => update }
      transmission.remoteLeft += { _ => update }
      update

      Signal { option() map { _() } }
    }
  }

  implicit class RescalaSignalSingleTransmissionProvider
      [Sig[T, ES <: Struct] <: EngineSignal[T, ES], T,
       R <: Peer, L <: Peer, ES <: Struct]
      (transmission: SingleTransmission[Sig[T, ES], R, L])
      (implicit val engine: Engine[ES, Turn[ES]])
    extends TransmissionProvider {
    import engine._

    lazy val optional =
      RescalaSignalOptionalTransmissionProvider(transmission.optional)

    lazy val multiple =
      RescalaSignalMultipleTransmissionProvider(transmission.multiple)

    lazy val asLocal: Signal[T] =  transmission.memo(asLocalId) {
      Signals.fromFuture(transmission.retrieveRemoteValue).flatten
    }
  }
}
