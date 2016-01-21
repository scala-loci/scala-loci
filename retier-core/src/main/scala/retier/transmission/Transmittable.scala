package retier
package transmission

import scala.annotation.implicitNotFound

@implicitNotFound("${T} is not transmittable")
sealed trait Transmittable[T, S, R] {
  protected final implicit class TransmittableOps[OpsT, OpsS, OpsR](
      transmittable: Transmittable[OpsT, OpsS, OpsR])
    extends transmittableMarshalling.TransmittableOps(transmittable)
}

abstract class PushBasedTransmittable[T, T0, S, R0, R](implicit
  private[retier] val transmittable: Transmittable[T0, S, R0],
  private[retier] val serializable: Serializable[S])
    extends Transmittable[T, S, R] {
  private[retier] val marshallable =
    Marshallable marshallable (transmittable, serializable)

  def send(value: T, remote: RemoteRef, sending: Sending[T0]): T0
  def receive(value: R0, remote: RemoteRef, receiving: Receiving[R0]): R
}

abstract class PullBasedTransmittable[T, S, R] extends Transmittable[T, S, R] {
  def send(value: T, remote: RemoteRef): S
  def receive(value: S, remote: RemoteRef): R
}


@implicitNotFound("${T} is not identically transmittable")
trait IdenticallyTransmittable[T] {
  def apply(): PullBasedTransmittable[T, T, T]
}

object IdenticallyTransmittable {
  def apply[T] = singletonPullBasedIdenticallyTransmittable.asInstanceOf[
    PullBasedIdenticallyTransmittable[T]]

  implicit def transmittable[T, V]
    (implicit
        transmittable: AnyTransmittable[T, V],
        identicallyTransmittable: V <:< PullBasedIdenticallyTransmittable[T]) =
    new IdenticallyTransmittable[T] {
      def apply() = identicallyTransmittable(transmittable())
    }

  sealed class PullBasedIdenticallyTransmittable[T]
      extends PullBasedTransmittable[T, T, T] {
    def send(value: T, remote: RemoteRef) = value
    def receive(value: T, remote: RemoteRef) = value
  }

  private[this] final val singletonPullBasedIdenticallyTransmittable =
    new PullBasedIdenticallyTransmittable[Any]

  sealed trait AnyTransmittable[T, V] {
    def apply(): V
  }

  implicit def anyTransmittable[T, S, R]
    (implicit transmittable: Transmittable[T, S, R]) =
    new AnyTransmittable[T, transmittable.type] {
      def apply() = transmittable
    }
}


trait TransmittableIdentity {
  implicit def identical[T] = IdenticallyTransmittable[T]
}

object Transmittable extends TransmittableCollections with TransmittableTuples
