package loci
package transmitter
package transmittable

trait Endpoint[T, U] {
  val closed: Notice.Steady[Unit]
  def close(): Unit
  def send(value: T): Unit
  val receive: Notice.Stream[U]
}
