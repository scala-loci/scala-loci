package retier
package serializable

import _root_.upickle.default._
import transmission.Serializable
//import transmission.Transmittable
//import transmission.transmittableMarshalling.TransmittableOps
import scala.util.Try

object upickle {
  implicit def upickleBasedSerializable[T]
      (implicit reader: Reader[T], writer: Writer[T]) = new Serializable[T] {
    def serialize(value: T) = write(value)(writer)
    def deserialize(value: String) = Try { read(value)(reader) }
  }

//  hook back into the transmittable models
//  this would make the serializable concept only usable in the context of
//  transmissions, which should not be the case
//
//  implicit def transmittableBasedReader[T, S]
//      (implicit transmittable: Transmittable[T, S, T], reader: Reader[S]) =
//    Reader[T] { case expr =>
//      transmittable receive (reader read expr)
//    }
//
//  implicit def transmittableBasedWriter[T, S]
//      (implicit transmittable: Transmittable[T, S, T], writer: Writer[S]) =
//    Writer[T] { case expr =>
//      writer write (transmittable send (expr))
//    }
}
