package loci
package transmitter
package transmittable

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait TransmittableGeneralIterableCollections extends TransmittableDummy {
  this: TransmittableBase.type =>

  final implicit def traversable[B, I, R, V[T] >: Null <: TraversableLike[T, V[T]]]
    (implicit
        transmittable: Transmittable[B, I, R],
        cbfI: CanBuildFrom[V[B], I, V[I]],
        cbfR: CanBuildFrom[V[I], R, V[R]])
  : DelegatingTransmittable[V[B], V[I], V[R]] {
      type Delegates = transmittable.Type
    } =
    DelegatingTransmittable(
      provide = (value, context) =>
        if (value == null) null else value map { context delegate _ },
      receive = (value, context) =>
        if (value == null) null else value map { context delegate _ })
}

trait TransmittableIterableCollections extends TransmittableGeneralCollections {
  this: TransmittableBase.type =>

  final implicit def identicalTraversable
    [T: IdenticallyTransmittable, V[T] <: TraversableLike[T, V[T]]]
  : IdenticallyTransmittable[V[T]] = IdenticallyTransmittable()
}
