package loci
package runtime

import loci.contexts.Immediate.Implicits.global
import loci.transmitter._

import scala.concurrent.Future
import scala.util.Success

object Marshallables {
  object unit extends Marshallable[Unit, Unit, Future[Unit]] {
    def marshal(value: Unit, abstraction: AbstractionRef) =
      MessageBuffer.empty
    def unmarshal(value: MessageBuffer, abstraction: AbstractionRef) =
      Success(())
    def unmarshal(value: Future[MessageBuffer], abstraction: AbstractionRef) =
      value map { _ => () }
    def connected = false
  }

  object nothing extends Marshallable[Nothing, Nothing, Future[Nothing]] {
    def nothing = throw new RemoteAccessException("Unexpected value of bottom type")
    def marshal(value: Nothing, abstraction: AbstractionRef) =
      nothing
    def unmarshal(value: MessageBuffer, abstraction: AbstractionRef) =
      nothing
    def unmarshal(value: Future[MessageBuffer], abstraction: AbstractionRef) =
      value map { _ => nothing }
    def connected = false
  }
}
