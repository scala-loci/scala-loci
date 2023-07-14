package loci.valueref

import loci.MessageBuffer
import loci.language.erased
import loci.language.impl
import loci.runtime.Peer
import loci.runtime.Peer.Signature
import loci.transmitter
import loci.transmitter.transmittable.IdenticallyTransmittable
import loci.transmitter.Serializable
import transmitter.Parser._

import java.util.UUID
import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.util.Try

case class ValueRef[+V, +P](
  peerId: UUID,
  valueId: UUID,
  signature: Peer.Signature
) {
  def at[R]: Option[ValueRef[V, R]] = macro impl.ValueRef.at[V, R]
}

object ValueRef {

  @compileTimeOnly("Call only allowed in multitier code. Use `ref.at[P]` instead.")
  def cast[V, P](ref: ValueRef[V, _]): Option[ValueRef[V, P]] = erased

  implicit def transmittable[V, P]: IdenticallyTransmittable[ValueRef[V, P]] = IdenticallyTransmittable()

  def serializeRef[V, P](value: ValueRef[V, P]): Serializer = elements(
    string(value.peerId.toString),
    string(value.valueId.toString),
    Signature.serialize(value.signature)
  )

  def deserializeRef[V, P](value: String): ValueRef[V, P] = {
    val Seq(peerId, valueId, signature) = parse(value).asElements(3): @unchecked
    ValueRef(
      UUID.fromString(peerId.asString),
      UUID.fromString(valueId.asString),
      Signature.deserialize(signature.asString).get
    )
  }

  implicit def serializable[V, P]: Serializable[ValueRef[V, P]] = new Serializable[ValueRef[V, P]] {

    override def serialize(value: ValueRef[V, P]): MessageBuffer = {
      MessageBuffer.encodeString(serializeRef(value).toString)
    }

    override def deserialize(value: MessageBuffer): Try[ValueRef[V, P]] = Try {
      deserializeRef[V, P](value.decodeString)
    }
  }

  implicit def serializableOptionValueRefOptionString[V, P]: Serializable[(Option[ValueRef[V, P]], Option[String])] = {
    val separator = ";"

    new Serializable[(Option[ValueRef[V, P]], Option[String])] {
      override def serialize(value: (Option[ValueRef[V, P]], Option[String])): MessageBuffer = {
        MessageBuffer.encodeString(
          elements(list(value._1.map(serializeRef).toList), list(value._2.map(string).toList)).toString
        )
      }

      override def deserialize(value: MessageBuffer): Try[(Option[ValueRef[V, P]], Option[String])] = Try {
        val Seq(refOpt, strOpt) = parse(value.decodeString).asElements(2): @unchecked
        (refOpt.asList.headOption.map(_.asString).map(deserializeRef), strOpt.asList.headOption.map(_.asString))
      }
    }
  }
}
