package retier
package transmission

sealed trait Transmission[T, R <: Peer, L <: Peer, M <: ConnectionMultiplicity]


sealed trait MultipleTransmission[T, R <: Peer, L <: Peer]
  extends Transmission[T, R, L, MultipleConnection]

sealed trait OptionalTransmission[T, R <: Peer, L <: Peer]
  extends Transmission[T, R, L, OptionalConnection]

sealed trait SingleTransmission[T, R <: Peer, L <: Peer]
  extends Transmission[T, R, L, SingleConnection]


private trait MultipleTransmissionImplBase[T, R <: Peer, L <: Peer]
  extends MultipleTransmission[T, R, L]

private trait OptionalTransmissionImplBase[T, R <: Peer, L <: Peer]
  extends OptionalTransmission[T, R, L]

private trait SingleTransmissionImplBase[T, R <: Peer, L <: Peer]
  extends SingleTransmission[T, R, L]
