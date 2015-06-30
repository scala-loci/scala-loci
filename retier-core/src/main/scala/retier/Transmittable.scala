package retier

sealed trait Transmittable[T, R <: Peer, L <: Peer, M <: ConnectionMultiplicity]


sealed trait MultipleTransmittable[T, R <: Peer, L <: Peer]
  extends Transmittable[T, R, L, MultipleConnection]

sealed trait OptionalTransmittable[T, R <: Peer, L <: Peer]
  extends Transmittable[T, R, L, OptionalConnection]

sealed trait SingleTransmittable[T, R <: Peer, L <: Peer]
  extends Transmittable[T, R, L, SingleConnection]


private trait MultipleTransmittableImplBase[T, R <: Peer, L <: Peer]
  extends MultipleTransmittable[T, R, L]

private trait OptionalTransmittableImplBase[T, R <: Peer, L <: Peer]
  extends OptionalTransmittable[T, R, L]

private trait SingleTransmittableImplBase[T, R <: Peer, L <: Peer]
  extends SingleTransmittable[T, R, L]
