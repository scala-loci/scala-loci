package loci
package serializer

import io.circe._
import io.circe.parser._
import io.circe.syntax._
import loci.transmitter.Serializable
import scala.util.Failure
import scala.util.Success

object circe {
  implicit def circeBasedSerializable[T]
      (implicit enc: Encoder[T], dec: Decoder[T]) = new Serializable[T] {
    override def serialize(value: T) =
      MessageBuffer.fromString(value.asJson.noSpaces)
    override def deserialize(value: MessageBuffer) =
      decode[T](value toString (0, value.length)) match {
        case Left(error) => Failure(error)
        case Right(res) => Success(res)
      }
  }
}
