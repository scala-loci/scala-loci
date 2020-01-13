package loci

import loci.transmitter._

import scala.util.Try

package object Testing {
  implicit object IntSerializable extends Serializable[Int] {
    def serialize(value: Int) =
      MessageBuffer.encodeString(value.toString)
    def deserialize(value: MessageBuffer) =
      Try { value.decodeString.toInt }
  }

  implicit class MultipleAccessor[V, R, T, L](value: V from R)(
      implicit ev: Transmission[V, R, T, L, transmitter.Multiple])
    extends RemoteAccessor {

    def asLocalFromAll: Seq[T] = value.retrieveValues
  }

  implicit class OptionalAccessor[V, R, T, L](value: V from R)(
      implicit ev: Transmission[V, R, T, L, transmitter.Optional])
    extends RemoteAccessor {

    def asLocal: Option[T] = value.retrieveValue
  }

  implicit class SingleAccessor[V, R, T, L](value: V from R)(
      implicit ev: Transmission[V, R, T, L, transmitter.Single])
    extends RemoteAccessor {

    def asLocal: T = value.retrieveValue
  }
}
