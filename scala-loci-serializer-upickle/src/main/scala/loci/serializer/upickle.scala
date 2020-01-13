package loci
package serializer

import _root_.upickle.default._
import transmitter.Serializable
import scala.util.Try

object upickle {
  implicit def upickleBasedSerializable[T]
      (implicit reader: Reader[T], writer: Writer[T]) = new Serializable[T] {
    def serialize(value: T) =
      MessageBuffer.encodeString(write(value)(writer))
    def deserialize(value: MessageBuffer) =
      Try { read(value.decodeString)(reader) }
  }
}
