package loci

import transmission.MultipleTransmission
import transmission.OptionalTransmission
import transmission.SingleTransmission
import transmission.TransmissionProvider
import transmission.Transmittable
import scala.language.implicitConversions

protected object ImplicitTransmissions {
  trait RemoteValue {
    implicit def transmitMultiple
      [V <: loci.RemoteValue, T, S, U, L <: Peer, R <: Peer,
       Provider <: TransmissionProvider]
      (v: V)
      (implicit
          ev0: TransmissionProperties[V, T, R, L, MultipleConnection],
          ev1: Transmittable[T, S, U],
          ev2: MultipleTransmission[U, R, L] => Provider): Provider = `#macro`

    implicit def transmitOptional
      [V <: loci.RemoteValue, T, S, U, L <: Peer, R <: Peer,
       Provider <: TransmissionProvider]
      (v: V)
      (implicit
          ev0: TransmissionProperties[V, T, R, L, OptionalConnection],
          ev1: Transmittable[T, S, U],
          ev2: OptionalTransmission[U, R, L] => Provider): Provider = `#macro`

    implicit def transmitSingle
      [V <: loci.RemoteValue, T, S, U, L <: Peer, R <: Peer,
       Provider <: TransmissionProvider]
      (v: V)
      (implicit
          ev0: TransmissionProperties[V, T, R, L, SingleConnection],
          ev1: Transmittable[T, S, U],
          ev2: SingleTransmission[U, R, L] => Provider): Provider = `#macro`
  }
}
