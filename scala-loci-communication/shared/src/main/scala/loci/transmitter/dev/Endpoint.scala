package loci
package transmitter
package dev

trait Endpoint[T, U] {
  val receive: Notification[U]
  def send(value: T): Unit
}
