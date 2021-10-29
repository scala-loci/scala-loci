package loci.valueref

import loci.MessageBuffer
import loci.transmitter.transmittable.IdenticallyTransmittable
import loci.transmitter.Serializable

import java.util.UUID
import scala.util.Try

case class ValueRef[V, P](peerId: UUID, valueId: UUID)

object ValueRef {
  implicit def transmittable[V, P]: IdenticallyTransmittable[ValueRef[V, P]] = IdenticallyTransmittable()

  implicit def serializable[V, P]: Serializable[ValueRef[V, P]] = new Serializable[ValueRef[V, P]] {
    private val separator = ","

    override def serialize(value: ValueRef[V, P]): MessageBuffer = {
      MessageBuffer.encodeString(Seq(value.peerId, value.valueId).map(_.toString).mkString(separator))
    }

    override def deserialize(value: MessageBuffer): Try[ValueRef[V, P]] = Try {
      val fields = value.decodeString.split(separator)
      ValueRef(UUID.fromString(fields(0)), UUID.fromString(fields(1)))
    }
  }

  implicit def serializableOptionValueRefOptionString[V, P]: Serializable[(Option[ValueRef[V, P]], Option[String])] = {
    val separator = ";"

    new Serializable[(Option[ValueRef[V, P]], Option[String])] {
      override def serialize(value: (Option[ValueRef[V, P]], Option[String])): MessageBuffer = {
        val (ref, string) = value
        val serializedRef = "[" + ref.map(serializable[V, P].serialize(_)).map(_.decodeString).getOrElse("") + "]"
        MessageBuffer.encodeString(serializedRef + string.map(s => separator + s).getOrElse(""))
      }

      override def deserialize(value: MessageBuffer): Try[(Option[ValueRef[V, P]], Option[String])] = Try {
        val decodedValue = value.decodeString
        val pair = decodedValue.indexOf(separator) match {
          case -1 => (decodedValue, None)
          case i => (decodedValue.slice(0, i), Some(decodedValue.slice(i + 1, decodedValue.length)))
        }
        pair match {
          case (ref, string) if ref == "[]" => (None, string)
          case (ref, string) => (
            serializable[V, P].deserialize(MessageBuffer.encodeString(ref.stripPrefix("[").stripSuffix("]"))).toOption,
            string
          )
        }
      }
    }
  }
}
