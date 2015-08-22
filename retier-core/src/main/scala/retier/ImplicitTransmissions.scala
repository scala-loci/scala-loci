package retier

import transmission.MultipleTransmission
import transmission.OptionalTransmission
import transmission.SingleTransmission
import transmission.TransmissionProvider
import scala.language.implicitConversions

protected trait ImplicitTransmissions {
  implicit def transmitMultiple
    [V, T, L <: Peer, R <: Peer, Provider <: TransmissionProvider]
    (v: V)
    (implicit
        ev0: TransmissionProperties[V, T, R, L, MultipleConnection],
        ev1: MultipleTransmission[T, R, L] => Provider): Provider = `#macro`

  implicit def transmitOptional
    [V, T, L <: Peer, R <: Peer, Provider <: TransmissionProvider]
    (v: V)
    (implicit
        ev0: TransmissionProperties[V, T, R, L, OptionalConnection],
        ev1: OptionalTransmission[T, R, L] => Provider): Provider = `#macro`

  implicit def transmitSingle
    [V, T, L <: Peer, R <: Peer, Provider <: TransmissionProvider]
    (v: V)
    (implicit
        ev0: TransmissionProperties[V, T, R, L, SingleConnection],
        ev1: SingleTransmission[T, R, L] => Provider): Provider = `#macro`
}
