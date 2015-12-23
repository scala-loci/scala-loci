package retier

import transmission.MultipleTransmission
import transmission.OptionalTransmission
import transmission.SingleTransmission
import transmission.TransmissionProvider
import transmission.Transmittable
import scala.language.implicitConversions

protected trait ImplicitTransmissions {
  implicit def $$retier$transmitMultiple
    [V, T, S, U, L <: Peer, R <: Peer, Provider <: TransmissionProvider]
    (v: V)
    (implicit
        ev0: TransmissionProperties[V, T, R, L, MultipleConnection],
        ev1: Transmittable[T, S, U],
        ev2: MultipleTransmission[U, R, L] => Provider): Provider = `#macro`

  implicit def $$retier$transmitOptional
    [V, T, S, U, L <: Peer, R <: Peer, Provider <: TransmissionProvider]
    (v: V)
    (implicit
        ev0: TransmissionProperties[V, T, R, L, OptionalConnection],
        ev1: Transmittable[T, S, U],
        ev2: OptionalTransmission[U, R, L] => Provider): Provider = `#macro`

  implicit def $$retier$transmitSingle
    [V, T, S, U, L <: Peer, R <: Peer, Provider <: TransmissionProvider]
    (v: V)
    (implicit
        ev0: TransmissionProperties[V, T, R, L, SingleConnection],
        ev1: Transmittable[T, S, U],
        ev2: SingleTransmission[U, R, L] => Provider): Provider = `#macro`
}
