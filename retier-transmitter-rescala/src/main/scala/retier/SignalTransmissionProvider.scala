package retier

import transmission._
import contexts.Immediate.Implicits.global
import rescala.turns.Engine
import rescala.turns.Turn
import rescala.graph.Spores
import rescala.{ Signal => EngineSignal }
import scala.util.Success
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext
import scala.language.higherKinds

protected[retier] trait SignalTransmissionProvider extends SignalDefaultValues {
  private final val asLocalId = 0

  implicit class RescalaSignalMultipleTransmissionProvider
      [Sig[T, ES <: Spores] <: EngineSignal[T, ES], T,
       R <: Peer, L <: Peer, ES <: Spores]
      (transmission: MultipleTransmission[Sig[T, ES], R, L])
      (implicit val engine: Engine[ES, Turn[ES]])
    extends TransmissionProvider {
    import engine._

    def asLocal: Signal[Map[Remote[R], Signal[T]]] =
      transmission.memo(asLocalId) {
        val mapping = Var(Map.empty[Remote[R], Signal[T]])

        def update() = {
          // if REScala signals could capture the state of not being evaluated
          // yet, we would not need to filter the mapping
          mapping() = transmission.retrieveMappedRemoteValues mapValues {
            _.value
          } collect {
            case (remote, Some(Success(signal))) => (remote, signal)
          }
        }

        transmission.retrieveRemoteValues foreach {
          _.onComplete { _ => update }
        }

        transmission.remoteJoined += { remote =>
          transmission.retrieveMappedRemoteValues get remote foreach {
            _.onComplete { _ => update }
          }
          update
        }

        transmission.remoteLeft += { _ => update }

        mapping
      }
    }

  implicit class RescalaSignalOptionalTransmissionProvider
      [Sig[T, ES <: Spores] <: EngineSignal[T, ES], T,
       R <: Peer, L <: Peer, ES <: Spores]
      (transmission: OptionalTransmission[Sig[T, ES], R, L])
      (implicit val engine: Engine[ES, Turn[ES]])
    extends TransmissionProvider {
    import engine._

    def multiple =
      RescalaSignalMultipleTransmissionProvider(transmission.multiple)

    def asLocal: Signal[Option[T]] = transmission.memo(asLocalId) {
      val option = Var(Option.empty[Signal[T]])

      def update() = {
        option() = transmission.retrieveRemoteValue flatMap {
          // if REScala signals could capture the state of not being evaluated
          // yet, we would not need to filter the option
          _.value match {
            case Some(Success(signal)) => Some(signal)
            case _ => None
          }
        }
      }

      transmission.retrieveRemoteValue foreach {
        _.onComplete { _ => update }
      }

      transmission.remoteJoined += { _ =>
        transmission.retrieveRemoteValue foreach {
          _.onComplete { _ => update }
        }
        update
      }

      transmission.remoteLeft += { _ => update }

      Signal { option() map { _() } }
    }
  }

  protected class RescalaSignalSingleTransmissionProviderCommon
      [Sig[T, ES <: Spores] <: EngineSignal[T, ES], T,
       R <: Peer, L <: Peer, ES <: Spores]
      (transmission: SingleTransmission[Sig[T, ES], R, L])
      (implicit val engine: Engine[ES, Turn[ES]])
    extends TransmissionProvider {
    import engine._

    def optional =
      RescalaSignalOptionalTransmissionProvider(transmission.optional)

    def multiple =
      RescalaSignalMultipleTransmissionProvider(transmission.multiple)

    def asLocal_?(timeout: Duration): Signal[T] = {
      // if REScala signals could capture the state of not being evaluated yet,
      // we would not need to block
      Await result (transmission.retrieveRemoteValue, timeout)
    }

    def asLocal_! : Signal[T] = asLocal_?(Duration.Inf)

    def asLocalOption: Signal[Option[T]] = transmission.memo(asLocalId) {
      val option = Var(Option.empty[Signal[T]])

      transmission.retrieveRemoteValue foreach { signal =>
        option() = Some(signal)
      }

      Signal { option() map { _() } }
    }

    def asLocal(default: T): Signal[T] =
      transmission.memo((asLocalId, default)) {
        Signal { asLocalOption() getOrElse default }
      }
  }

  implicit class RescalaSignalSingleTransmissionProviderWithDefaultValue
      [Sig[T, ES <: Spores] <: EngineSignal[T, ES], T: SignalDefaultValue,
       R <: Peer, L <: Peer, ES <: Spores]
      (transmission: SingleTransmission[Sig[T, ES], R, L])
      (implicit override val engine: Engine[ES, Turn[ES]])
    extends RescalaSignalSingleTransmissionProviderCommon
      [Sig, T, R, L, ES](transmission) {
    import engine._

    def asLocal: Signal[T] = asLocal(implicitly[SignalDefaultValue[T]].value)
  }

  implicit class RescalaSignalSingleTransmissionProviderWithoutDefaultValue
      [Sig[T, ES <: Spores] <: EngineSignal[T, ES], T: NoSignalDefaultValue,
       R <: Peer, L <: Peer, ES <: Spores]
      (transmission: SingleTransmission[Sig[T, ES], R, L])
      (implicit override val engine: Engine[ES, Turn[ES]])
    extends RescalaSignalSingleTransmissionProviderCommon
      [Sig, T, R, L, ES](transmission) {
    import engine._

    def asLocal: Signal[Option[T]] = asLocalOption
  }
}
