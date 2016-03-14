package retier

import transmission._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object basicTransmitter extends ide.intellij.BasicTransmitter {
  implicit class BasicMultipleTransmissionProvider
      [T, R <: Peer, L <: Peer]
      (transmission: MultipleTransmission[T, R, L])
    extends TransmissionProvider {

    def asLocal: Map[Remote[R], Future[T]] =
      transmission.retrieveMappedRemoteValues

    def asLocal_?(timeout: Duration): Map[Remote[R], T] =
      transmission.retrieveMappedRemoteValues map { case (remote, future) =>
        remote -> (Await result (future, timeout))
      }

    def asLocal_! : Map[Remote[R], T] = asLocal_?(Duration.Inf)
  }

  implicit class BasicOptionalTransmissionProvider
      [T, R <: Peer, L <: Peer]
      (transmission: OptionalTransmission[T, R, L])
    extends TransmissionProvider {

    def multiple = BasicMultipleTransmissionProvider(transmission.multiple)

    def asLocal: Option[Future[T]] =
      transmission.retrieveRemoteValue

    def asLocal_?(timeout: Duration): Option[T] =
      transmission.retrieveRemoteValue map { Await result (_, timeout) }

    def asLocal_! : Option[T] = asLocal_?(Duration.Inf)
  }

  implicit class BasicSingleTransmissionProvider
      [T, R <: Peer, L <: Peer]
      (transmission: SingleTransmission[T, R, L])
    extends TransmissionProvider {

    def optional = BasicOptionalTransmissionProvider(transmission.optional)

    def multiple = BasicMultipleTransmissionProvider(transmission.multiple)

    def asLocal: Future[T] =
      transmission.retrieveRemoteValue

    def asLocal_?(timeout: Duration): T =
      Await result (transmission.retrieveRemoteValue, timeout)

    def asLocal_! : T = asLocal_?(Duration.Inf)
  }
}
