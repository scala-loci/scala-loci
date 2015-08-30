package retier
package impl

import transmission.AbstractionId
import transmission.Marshallable

trait TransmissionProperties[T, U] {
  def abstraction: AbstractionId
  def marshallable: Option[Marshallable[T]]
  def request: Option[(Marshallable[U], U)]
}

private final case class RequestPropertiesImpl[T, U](abstraction: AbstractionId,
    marshallable: Option[Marshallable[T]],
    request: Option[(Marshallable[U], U)])
  extends TransmissionProperties[T, U]

object TransmissionProperties {
  def create[T, U](abstraction: AbstractionId,
  	  marshallable: Option[Marshallable[T]],
  	  request: Option[(Marshallable[U], U)]): TransmissionProperties[T, U] =
    RequestPropertiesImpl(abstraction, marshallable, request)
}
