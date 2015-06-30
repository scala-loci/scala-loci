package retier

import scala.language.implicitConversions

protected trait ImplicitTransmissions {
  implicit def transmitMultiple
    [V, T, L <: Peer, R <: Peer, Trans <: Transmitter]
    (v: V)
    (implicit
        ev0: Transmission[V, T, R, L, MultipleConnection],
        ev1: MultipleTransmittable[T, R, L] => Trans): Trans = `#macro`

  implicit def transmitOptional
    [V, T, L <: Peer, R <: Peer, Trans <: Transmitter]
    (v: V)
    (implicit
        ev0: Transmission[V, T, R, L, OptionalConnection],
        ev1: OptionalTransmittable[T, R, L] => Trans): Trans = `#macro`

  implicit def transmitSingle
    [V, T, L <: Peer, R <: Peer, Trans <: Transmitter]
    (v: V)
    (implicit
        ev0: Transmission[V, T, R, L, SingleConnection],
        ev1: SingleTransmittable[T, R, L] => Trans): Trans = `#macro`
}
