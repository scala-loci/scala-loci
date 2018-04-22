package loci

import transmission._
import contexts.Immediate.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object basicTransmitter extends ide.intellij.BasicTransmitter {
  implicit class BasicMultipleTransmissionProvider
      [T, R <: Peer, L <: Peer]
      (transmission: MultipleTransmission[T, R, L])
    extends TransmissionProvider {

    def asLocalFromAll: Map[Remote[R], Future[T]] =
      transmission.retrieveMappedRemoteValues

    def asLocalFromAll_?(timeout: Duration): Map[Remote[R], T] = {
      val (remotes, values) = transmission.retrieveMappedRemoteValues.unzip
      (remotes zip (Await result (Future sequence values, timeout))).toMap
    }

    def asLocalFromAll_! : Map[Remote[R], T] = asLocalFromAll_?(Duration.Inf)
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
