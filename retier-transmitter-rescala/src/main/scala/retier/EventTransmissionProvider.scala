package retier

import transmission._
import contexts.Immediate.Implicits.global
import rescala.synchronization.Engines.default
import rescala.synchronization.Engines.default._
import scala.util.Success
import scala.concurrent.ExecutionContext
import scala.language.higherKinds

protected[retier] trait EventTransmissionProvider {
  private final val asLocalId = 0
  private final val asLocalSeqId = 1

  implicit class RescalaEventMultipleTransmissionProvider
      [Evnt[T] <: Event[T], T, R <: Peer, L <: Peer]
      (transmission: MultipleTransmission[Evnt[T], R, L])
    extends TransmissionProvider {

    def asLocal: Signal[Map[Remote[R], Event[T]]] =
      transmission.memo(asLocalId) {
        val mapping = Var(Map.empty[Remote[R], Event[T]])

        def update() = {
          mapping() = transmission.retrieveMappedRemoteValues mapValues {
            _.value
          } collect {
            case (remote, Some(Success(event))) => (remote, event)
            case (remote, _) => (remote, Evt())
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
          event map { (remote, _) }
        } reduceOption {
          _ || _
        } getOrElse Evt()
      }).unwrap
    }
  }

  implicit class RescalaEventOptionalTransmissionProvider
      [Evnt[T] <: Event[T], T, R <: Peer, L <: Peer]
      (transmission: OptionalTransmission[Evnt[T], R, L])
    extends TransmissionProvider {

    def multiple =
      RescalaEventMultipleTransmissionProvider(transmission.multiple)

    def asLocal: Signal[Option[Event[T]]] = transmission.memo(asLocalId) {
      val option = Var(Option.empty[Event[T]])

      def update() = {
        option() = transmission.retrieveRemoteValue map {
          _.value match {
            case Some(Success(event)) => event
            case _ => Evt()
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
      (Signal { asLocal() getOrElse Evt() }).unwrap
    }
  }

  implicit class RescalaEventSingleTransmissionProvider
      [Evnt[T] <: Event[T], T, R <: Peer, L <: Peer]
      (transmission: SingleTransmission[Evnt[T], R, L])
    extends TransmissionProvider {

    def optional =
      RescalaEventOptionalTransmissionProvider(transmission.optional)

    def multiple =
      RescalaEventMultipleTransmissionProvider(transmission.multiple)

    def asLocal: Event[T] = transmission.memo(asLocalId) {
      val event = Evt[T]
      transmission.retrieveRemoteValue foreach { _ += event.apply }
      event
    }
  }
}
