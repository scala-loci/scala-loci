package retier

import transmission._
import rescala.Signal
import rescala.Var
import makro.SignalMacro.{SignalM => Signal}
import scala.util.Success
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext
import scala.language.higherKinds

protected[retier] trait SignalTransmissionProvider {
  private implicit val executionContext = new ExecutionContext {
    def execute(runnable: Runnable) = runnable.run
    def reportFailure(throwable: Throwable) =
      ExecutionContext.defaultReporter(throwable)
  }

  implicit class RescalaSignalMultipleTransmissionProvider
      [Sig[T] <: Signal[T], T, R <: Peer, L <: Peer]
      (transmission: MultipleTransmission[Sig[T], R, L])
    extends TransmissionProvider {

    def asLocal: Signal[Map[Remote[R], Signal[T]]] = {
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
      [Sig[T] <: Signal[T], T, R <: Peer, L <: Peer]
      (transmission: OptionalTransmission[Sig[T], R, L])
    extends TransmissionProvider {

    def asLocal: Signal[Option[T]] = {
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

  implicit class RescalaSignalSingleTransmissionProvider
      [Sig[T] <: Signal[T], T, R <: Peer, L <: Peer]
      (transmission: SingleTransmission[Sig[T], R, L])
    extends TransmissionProvider {

    def asLocal: Signal[T] = {
      // if REScala signals could capture the state of not being evaluated yet,
      // we would not need to block
      Await result (transmission.retrieveRemoteValue, Duration.Inf)
    }
  }
}
