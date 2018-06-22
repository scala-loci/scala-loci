package loci
package impl

import AbstractionRef._
import transmitter.Marshallable
import scala.concurrent.Future
import scala.util.Success

object UnitMarshallable extends Marshallable[Unit, Unit, Unit] {
  def marshal(value: Unit, abstraction: AbstractionRef): MessageBuffer =
    MessageBuffer.empty
  def unmarshal(value: MessageBuffer, abstraction: AbstractionRef) =
    Success(())
  def unmarshal(value: Future[MessageBuffer], abstraction: AbstractionRef) =
    ()

  def isPushBased = false
}
