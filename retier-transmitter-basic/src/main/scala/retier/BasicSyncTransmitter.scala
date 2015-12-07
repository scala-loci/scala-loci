package retier

import transmission._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object basicSyncTransmitter extends ide.intellij.BasicSyncTransmitter {
  implicit class BasicSyncMultipleTransmissionProvider
      [T, R <: Peer, L <: Peer]
      (transmission: MultipleTransmission[T, R, L])
    extends TransmissionProvider {

    def asLocal_! : Map[Remote[R], T] =
      transmission.retrieveMappedRemoteValues map { case (remote, future) =>
        remote -> (Await result (future, Duration.Inf))
      }
  }

  implicit class BasicSyncOptionalTransmissionProvider
      [T, R <: Peer, L <: Peer]
      (transmission: OptionalTransmission[T, R, L])
    extends TransmissionProvider {

    def asLocal_! : Option[T] =
      transmission.retrieveRemoteValue map { Await result (_, Duration.Inf) }
  }

  implicit class BasicSyncSingleTransmissionProvider
      [T, R <: Peer, L <: Peer]
      (transmission: SingleTransmission[T, R, L])
    extends TransmissionProvider {

    def asLocal_! : T =
      Await result (transmission.retrieveRemoteValue, Duration.Inf)
  }
}
