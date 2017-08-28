package loci
package impl

import AbstractionId._
import AbstractionRef._
import transmitter.Marshallable
import transmitter.MarshallableArgument
import scala.util.Try

trait TransmissionProperties[T] {
  def abstraction: AbstractionId
  def marshalRequest(abstraction: AbstractionRef): MessageBuffer
  def unmarshalResponse(response: MessageBuffer, abstraction: AbstractionRef): Try[T]
  def isStable: Boolean
  def isPushBased: Boolean
}

private final case class TransmissionPropertiesImpl[T, U](
  abstraction: AbstractionId,
  responseMarshallable: Marshallable[_] { type Result = T },
  request: U, requestMarshallable: MarshallableArgument[U])
    extends TransmissionProperties[T] {
  def marshalRequest(abstraction: AbstractionRef) =
    requestMarshallable marshal (request, abstraction)
  def unmarshalResponse(response: MessageBuffer, abstraction: AbstractionRef) =
    responseMarshallable unmarshal (response, abstraction)
  def isStable = abstraction.isStable
  def isPushBased = responseMarshallable.isPushBased
}

object TransmissionProperties {
  def create[T, U](
      abstraction: AbstractionId, responseMarshallable: Marshallable[T],
      request: U, requestMarshallable: MarshallableArgument[U])
  : TransmissionProperties[responseMarshallable.Result] =
    TransmissionPropertiesImpl(
      abstraction, responseMarshallable, request, requestMarshallable)
}
