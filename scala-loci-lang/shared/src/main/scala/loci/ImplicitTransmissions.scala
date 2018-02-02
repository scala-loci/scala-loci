package loci

import transmitter.MultipleTransmission
import transmitter.OptionalTransmission
import transmitter.SingleTransmission
import transmitter.TransmissionProvider
import transmitter.Transmittable
import scala.language.implicitConversions

protected object ImplicitTransmissions {
  trait RemoteValue {
    implicit def transmitMultiple
      [V <: loci.RemoteValue, T, S, U, L <: Peer, R <: Peer,
       Provider <: TransmissionProvider]
      (v: V)
      (implicit
          ev0: TransmissionProperties[V, T, R, L, MultipleTie],
          ev1: Transmittable[T, S, U],
          ev2: MultipleTransmission[U, R, L] => Provider): Provider = `#macro`(ev0, ev1, ev2)

    implicit def transmitOptional
      [V <: loci.RemoteValue, T, S, U, L <: Peer, R <: Peer,
       Provider <: TransmissionProvider]
      (v: V)
      (implicit
          ev0: TransmissionProperties[V, T, R, L, OptionalTie],
          ev1: Transmittable[T, S, U],
          ev2: OptionalTransmission[U, R, L] => Provider): Provider = `#macro`(ev0, ev1, ev2)

    implicit def transmitSingle
      [V <: loci.RemoteValue, T, S, U, L <: Peer, R <: Peer,
       Provider <: TransmissionProvider]
      (v: V)
      (implicit
          ev0: TransmissionProperties[V, T, R, L, SingleTie],
          ev1: Transmittable[T, S, U],
          ev2: SingleTransmission[U, R, L] => Provider): Provider = `#macro`(ev0, ev1, ev2)
  }
}
