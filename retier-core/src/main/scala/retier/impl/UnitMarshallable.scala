package retier
package impl

import AbstractionRef._
import transmission.MarshallableArgument
import scala.util.Success

object UnitMarshallable extends MarshallableArgument[Unit] {
  def marshal(unmarshalled: Unit, abstraction: AbstractionRef) = ""
  def unmarshal(marshalled: String, abstraction: AbstractionRef) = Success(())
  def isPushBased = false
}
