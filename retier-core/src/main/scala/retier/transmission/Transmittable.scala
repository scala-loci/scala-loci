package retier
package transmission

sealed trait Transmittable[T, U]

trait PullBasedTransmittable[T, U] extends Transmittable[T, U] {
  def send(value: T): U
  def receive(value: U): T
}

trait PushBasedTransmittable[T, U] extends Transmittable[T, U] {
  def send(value: T, sending: Sending[U]): U
  def receive(value: U, receiving: Receiving[U]): T
}
