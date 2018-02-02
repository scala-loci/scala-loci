package loci
package transmitter

import scala.annotation.implicitNotFound

@implicitNotFound("${T} is not transmittable")
sealed trait Transmittable[T, S, R] {
  protected final implicit class TransmittableOps[OpsT, OpsS, OpsR](
      transmittable: Transmittable[OpsT, OpsS, OpsR])
    extends transmittableMarshalling.TransmittableOps(transmittable)
}

abstract class PushBasedTransmittable[T, T0, S, R0, R](implicit
  private[loci] val transmittable: Transmittable[T0, S, R0],
  private[loci] val serializable: Serializable[S])
    extends Transmittable[T, S, R] {
  private[loci] val marshallable =
    Marshallable marshallable (transmittable, serializable)

  def send(value: T, remote: RemoteRef, endpoint: Endpoint[T0, R0]): T0
  def receive(value: R0, remote: RemoteRef, endpoint: Endpoint[T0, R0]): R
}

abstract class PullBasedTransmittable[T, S, R] extends Transmittable[T, S, R] {
  def send(value: T, remote: RemoteRef): S
  def receive(value: S, remote: RemoteRef): R
}


@implicitNotFound("${T} is not identically transmittable")
sealed trait IdenticallyTransmittable[T]


object IdenticallyTransmittable {
  def apply[T] = singletonPullBasedIdenticallyTransmittable.asInstanceOf[
    PullBasedIdenticallyTransmittable[T]]

  sealed class PullBasedIdenticallyTransmittable[T]
      extends PullBasedTransmittable[T, T, T] {
    def send(value: T, remote: RemoteRef) = value
    def receive(value: T, remote: RemoteRef) = value
  }

  private[this] final val singletonPullBasedIdenticallyTransmittable =
    new PullBasedIdenticallyTransmittable[Any]


  implicit def transmittable[T, V]
      (implicit
        transmittable: AnyTransmittable[T, V],
        identicallyTransmittable: V <:< PullBasedIdenticallyTransmittable[T]) = {
    locally(transmittable)
    locally(identicallyTransmittable)
    singletonIdenticallyTransmittable.asInstanceOf[IdenticallyTransmittable[T]]
  }

  private[this] final val singletonIdenticallyTransmittable =
    new IdenticallyTransmittable[Any] { }


  protected sealed trait AnyTransmittable[T, V]

  protected implicit def anyTransmittable[T, S, R]
      (implicit transmittable: Transmittable[T, S, R]) =
    null.asInstanceOf[AnyTransmittable[T, transmittable.type]]
}


trait TransmittableIdentity {
  implicit def identical[T] = IdenticallyTransmittable[T]
}

object Transmittable extends TransmittableCollections with TransmittableTuples
