package retier
package transmission

import scala.util.Try

trait Channel {
  def receive(handler: (String, String) => Unit): Unit
  def closed(handler: => Unit): Unit

  def send(messageType: String, payload: String): Unit
  def close(): Unit

  def abstraction: AbstractionRef

  final def marshall[T: Marshallable](value: T): String =
    implicitly[Marshallable[T]] marshall (value, abstraction)
  final def unmarshall[T: Marshallable](value: String): Try[T] =
    implicitly[Marshallable[T]] unmarshall (value, abstraction)

  final def closedException = new ChannelClosedException
}

class ChannelClosedException extends RuntimeException(
  "Channel was unexpectedly closed")
