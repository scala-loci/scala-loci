package loci
package transmission

import scala.collection.generic.CanBuildFrom
import scala.collection.TraversableLike
import scala.reflect.ClassTag
import scala.language.higherKinds

trait TransmittableNonNullableCollections extends TransmittableIdentity {
  implicit def nonNullableTraversable[T, S, R, V[T] <: TraversableLike[T, V[T]]]
    (implicit
        transmittable: Transmittable[T, S, R],
        cbfS: CanBuildFrom[V[T], S, V[S]],
        cbfR: CanBuildFrom[V[S], R, V[R]])
    : Transmittable[V[T], V[S], V[R]] =
    new PullBasedTransmittable[V[T], V[S], V[R]] {
      def send(value: V[T], remote: RemoteRef) =
        value map { transmittable send _ }
      def receive(value: V[S], remote: RemoteRef) =
        value map { transmittable receive _ }
    }
}

trait TransmittableGeneralCollections
    extends TransmittableNonNullableCollections {
  implicit def traversable[T, S, R, V[T] >: Null <: TraversableLike[T, V[T]]]
    (implicit
        transmittable: Transmittable[T, S, R],
        cbfS: CanBuildFrom[V[T], S, V[S]],
        cbfR: CanBuildFrom[V[S], R, V[R]])
    : Transmittable[V[T], V[S], V[R]] =
    new PullBasedTransmittable[V[T], V[S], V[R]] {
      def send(value: V[T], remote: RemoteRef) =
        if (value == null) null else value map { transmittable send _ }
      def receive(value: V[S], remote: RemoteRef) =
        if (value == null) null else value map { transmittable receive _ }
    }

  implicit def array[T: ClassTag, S: ClassTag, R: ClassTag]
    (implicit transmittable: Transmittable[T, S, R])
    : Transmittable[Array[T], Array[S], Array[R]] =
    new PullBasedTransmittable[Array[T], Array[S], Array[R]] {
      def send(value: Array[T], remote: RemoteRef) =
        if (value == null) null else value map { transmittable send _ }
      def receive(value: Array[S], remote: RemoteRef) =
        if (value == null) null else value map { transmittable receive _ }
    }

  implicit def map[KT, KS, KR, VT, VS, VR]
    (implicit
        transmittableKey: Transmittable[KT, KS, KR],
        transmittableValue: Transmittable[VT, VS, VR])
    : Transmittable[Map[KT, VT], Map[KS, VS], Map[KR, VR]] =
    new PullBasedTransmittable[Map[KT, VT], Map[KS, VS], Map[KR, VR]] {
      def send(value: Map[KT, VT], remote: RemoteRef) =
        if (value == null) null else value map { case (key, value) =>
          (transmittableKey send key, transmittableValue send value) }
      def receive(value: Map[KS, VS], remote: RemoteRef) =
        if (value == null) null else value map { case (key, value) =>
          (transmittableKey receive key, transmittableValue receive value) }
    }

  implicit def option[T, S, R]
    (implicit transmittable: Transmittable[T, S, R])
    : Transmittable[Option[T], Option[S], Option[R]] =
    new PullBasedTransmittable[Option[T], Option[S], Option[R]] {
      def send(value: Option[T], remote: RemoteRef) =
        if (value == null) null else value map { transmittable send _ }
      def receive(value: Option[S], remote: RemoteRef) =
        if (value == null) null else value map { transmittable receive _ }
    }

  implicit def some[T, S, R]
    (implicit transmittable: Transmittable[T, S, R])
    : Transmittable[Some[T], Some[S], Some[R]] =
    new PullBasedTransmittable[Some[T], Some[S], Some[R]] {
      def send(value: Some[T], remote: RemoteRef) =
        if (value == null) null else Some(transmittable send value.get)
      def receive(value: Some[S], remote: RemoteRef) =
        if (value == null) null else Some(transmittable receive value.get)
    }

  implicit def either[LT, LS, LR, RT, RS, RR]
    (implicit
        transmittableLeft: Transmittable[LT, LS, LR],
        transmittableRight: Transmittable[RT, RS, RR])
    : Transmittable[Either[LT, RT], Either[LS, RS], Either[LR, RR]] =
    new PullBasedTransmittable[Either[LT, RT], Either[LS, RS], Either[LR, RR]] {
      def send(value: Either[LT, RT], remote: RemoteRef) = value match {
        case null => null
        case Left(value) => Left(transmittableLeft send value)
        case Right(value) => Right(transmittableRight send value)
      }
      def receive(value: Either[LS, RS], remote: RemoteRef) = value match {
        case null => null
        case Left(value) => Left(transmittableLeft receive value)
        case Right(value) => Right(transmittableRight receive value)
      }
    }

  implicit def left[LT, LS, LR, RT, RS, RR]
    (implicit transmittable: Transmittable[LT, LS, LR])
    : Transmittable[Left[LT, RT], Left[LS, RS], Left[LR, RR]] =
    new PullBasedTransmittable[Left[LT, RT], Left[LS, RS], Left[LR, RR]] {
      def send(value: Left[LT, RT], remote: RemoteRef) =
        if (value == null) null else Left(transmittable send value.left.get)
      def receive(value: Left[LS, RS], remote: RemoteRef) =
        if (value == null) null else Left(transmittable receive value.left.get)
    }

  implicit def right[LT, LS, LR, RT, RS, RR]
    (implicit transmittable: Transmittable[RT, RS, RR])
    : Transmittable[Right[LT, RT], Right[LS, RS], Right[LR, RR]] =
    new PullBasedTransmittable[Right[LT, RT], Right[LS, RS], Right[LR, RR]] {
      def send(value: Right[LT, RT], remote: RemoteRef) =
        if (value == null) null else Right(transmittable send value.right.get)
      def receive(value: Right[LS, RS], remote: RemoteRef) =
        if (value == null) null else Right(transmittable receive value.right.get)
    }
}

trait TransmittableCollections extends TransmittableGeneralCollections {
  implicit def identicalTraversable
    [T: IdenticallyTransmittable, V[T] <: TraversableLike[T, V[T]]] =
    IdenticallyTransmittable[V[T]]

  implicit def identicalArray[T: IdenticallyTransmittable] =
    IdenticallyTransmittable[Array[T]]

  implicit def identicalMap
    [V: IdenticallyTransmittable, K: IdenticallyTransmittable] =
    IdenticallyTransmittable[Map[V, K]]

  implicit def identicalOption[T: IdenticallyTransmittable] =
    IdenticallyTransmittable[Option[T]]

  implicit def identicalSome[T: IdenticallyTransmittable] =
    IdenticallyTransmittable[Some[T]]

  implicit def identicalNone =
    IdenticallyTransmittable[None.type]

  implicit def identicalEither
    [L: IdenticallyTransmittable, R: IdenticallyTransmittable] =
    IdenticallyTransmittable[Either[L, R]]

  implicit def identicalLeft[L: IdenticallyTransmittable, R] =
    IdenticallyTransmittable[Left[L, R]]

  implicit def identicalRight[L, R: IdenticallyTransmittable] =
    IdenticallyTransmittable[Right[L, R]]
}
