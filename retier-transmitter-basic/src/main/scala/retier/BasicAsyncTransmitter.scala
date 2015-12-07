package retier

import transmission._
import scala.concurrent.Future

object basicAsyncTransmitter extends ide.intellij.BasicAsyncTransmitter {
  implicit class BasicAsyncMultipleTransmissionProvider
      [T, R <: Peer, L <: Peer]
      (transmission: MultipleTransmission[T, R, L])
    extends TransmissionProvider {

    def asLocal: Map[Remote[R], Future[T]] =
      transmission.retrieveMappedRemoteValues
  }

  implicit class BasicAsyncOptionalTransmissionProvider
      [T, R <: Peer, L <: Peer]
      (transmission: OptionalTransmission[T, R, L])
    extends TransmissionProvider {

    def asLocal: Option[Future[T]] =
      transmission.retrieveRemoteValue
  }

  implicit class BasicAsyncSingleTransmissionProvider
      [T, R <: Peer, L <: Peer]
      (transmission: SingleTransmission[T, R, L])
    extends TransmissionProvider {

    def asLocal: Future[T] =
      transmission.retrieveRemoteValue
  }
}
