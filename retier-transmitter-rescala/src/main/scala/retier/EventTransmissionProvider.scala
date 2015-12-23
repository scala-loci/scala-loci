package retier

import transmission._
import rescala.Signal
import rescala.Var
import rescala.events.Event
import rescala.events.ImperativeEvent
import rescala.events.emptyevent
import makro.SignalMacro.{SignalM => Signal}
import scala.util.Success
import scala.concurrent.ExecutionContext
import scala.language.higherKinds

protected[retier] trait EventTransmissionProvider {
  private implicit val executionContext = new ExecutionContext {
    def execute(runnable: Runnable) = runnable.run
    def reportFailure(throwable: Throwable) =
      ExecutionContext.defaultReporter(throwable)
  }

  private final val asLocalId = 0
  private final val asLocalSeqId = 1

  implicit class RescalaEventMultipleTransmissionProvider
      [Evt[T] <: Event[T], T, R <: Peer, L <: Peer]
      (transmission: MultipleTransmission[Evt[T], R, L])
    extends TransmissionProvider {

    def asLocal: Signal[Map[Remote[R], Event[T]]] =
      transmission.memo(asLocalId) {
        val mapping = Var(Map.empty[Remote[R], Event[T]])

        def update() = {
          mapping() = transmission.retrieveMappedRemoteValues mapValues {
            _.value
          } collect {
            case (remote, Some(Success(event))) => (remote, event)
            case (remote, _) => (remote, emptyevent)
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

    def asLocalSeq: Event[(Remote[R], T)] = transmission.memo(asLocalSeqId) {
      (Signal {
        asLocal() map { case (remote, event) =>
          event map { (remote, _: T) }
        } reduceOption {
          _ || _
        } getOrElse emptyevent
      }).unwrap
    }
  }

  implicit class RescalaEventOptionalTransmissionProvider
      [Evt[T] <: Event[T], T, R <: Peer, L <: Peer]
      (transmission: OptionalTransmission[Evt[T], R, L])
    extends TransmissionProvider {

    def asLocal: Signal[Option[Event[T]]] = transmission.memo(asLocalId) {
      val option = Var(Option.empty[Event[T]])

      def update() = {
        option() = transmission.retrieveRemoteValue map {
          _.value match {
            case Some(Success(event)) => event
            case _ => emptyevent
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

      option
    }

    def asLocalSeq: Event[T] = transmission.memo(asLocalSeqId) {
      (Signal { asLocal() getOrElse emptyevent }).unwrap
    }
  }

  implicit class RescalaEventSingleTransmissionProvider
      [Evt[T] <: Event[T], T, R <: Peer, L <: Peer]
      (transmission: SingleTransmission[Evt[T], R, L])
    extends TransmissionProvider {

    def asLocal: Event[T] = transmission.memo(asLocalId) {
      val event = new ImperativeEvent[T]
      transmission.retrieveRemoteValue onSuccess PartialFunction {
        _ += event.apply
      }
      event
    }
  }
}
