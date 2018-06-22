package loci
package impl

import AbstractionId._
import AbstractionRef._
import transmitter.Marshallable
import scala.util.Try

trait TransmissionProperties[T] {
  def abstraction: AbstractionId
  def marshalRequest(abstraction: AbstractionRef): MessageBuffer
  def unmarshalResponse(response: MessageBuffer, abstraction: AbstractionRef): Try[T]
  def isStable: Boolean
  def isPushBased: Boolean
}

private final case class TransmissionPropertiesImpl[B0, R0, P0, B1, P1](
  abstraction: AbstractionId,
  responseMarshallable: Marshallable[B0, R0, P0],
  request: B1, requestMarshallable: Marshallable[B1, B1, P1])
    extends TransmissionProperties[R0] {
  def marshalRequest(abstraction: AbstractionRef) =
    requestMarshallable marshal (request, abstraction)
  def unmarshalResponse(response: MessageBuffer, abstraction: AbstractionRef) =
    responseMarshallable unmarshal (response, abstraction)
  def isStable = abstraction.isStable
  def isPushBased = responseMarshallable.isPushBased
}

object TransmissionProperties {
  def create[T, U, R](
      abstraction: AbstractionId, responseMarshallable: Marshallable[T, R, _],
      request: U, requestMarshallable: Marshallable[U, U, _])
  : TransmissionProperties[R] =
    TransmissionPropertiesImpl(
      abstraction, responseMarshallable, request, requestMarshallable)
}
