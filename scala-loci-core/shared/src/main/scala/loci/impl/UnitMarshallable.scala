package loci
package impl

import AbstractionRef._
import transmitter.MarshallableArgument
import scala.util.Success

object UnitMarshallable extends MarshallableArgument[Unit] {
  def marshal(unmarshalled: Unit, abstraction: AbstractionRef) =
    MessageBuffer.empty

  def unmarshal(marshalled: MessageBuffer, abstraction: AbstractionRef) =
    Success(())

  def isPushBased = false
}
