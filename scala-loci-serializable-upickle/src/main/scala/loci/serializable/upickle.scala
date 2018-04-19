package loci
package serializable

import _root_.upickle.default._
import transmission.Serializable
import scala.util.Try

object upickle {
  implicit def upickleBasedSerializable[T]
      (implicit reader: Reader[T], writer: Writer[T]) = new Serializable[T] {
    def serialize(value: T) = write(value)(writer)
    def deserialize(value: String) = Try { read(value)(reader) }
  }
}
