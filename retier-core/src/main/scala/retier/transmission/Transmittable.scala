package retier
package transmission

sealed trait Transmittable[T, U]

trait PullBasedTransmittable[T, U] extends Transmittable[T, U] {
  def send(value: T): U
  def receive(value: U): T
}

trait PushBasedTransmittable[T, U] extends Transmittable[T, U] {
  def send(value: T, update: () => U, close: () => Unit): U
  def receive(value: U, updated: U => Unit, closed: () => Unit): T
}
