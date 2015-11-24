package retier
package impl

import transmission.AbstractionId
import transmission.AbstractionRef
import transmission.Marshallable
import transmission.MarshallableArgument

trait TransmissionProperties[T, R] {
  def abstraction: AbstractionId
  def marshallable: Marshallable[T] { type Result = R }
  def marshalRequest(abstraction: AbstractionRef): String
}

private final case class RequestPropertiesImpl[T, R, U](
  abstraction: AbstractionId, marshallable: Marshallable[T] { type Result = R },
  request: U, requestMarshallable: MarshallableArgument[U])
    extends TransmissionProperties[T, R] {
  def marshalRequest(abstraction: AbstractionRef) =
    requestMarshallable marshal (request, abstraction)
}

object TransmissionProperties {
  def create[T, U, R](
      abstraction: AbstractionId, marshallable: Marshallable[T],
      request: U, requestMarshallable: MarshallableArgument[U])
  : TransmissionProperties[T, marshallable.Result] =
    RequestPropertiesImpl(
      abstraction, marshallable, request, requestMarshallable)
}
