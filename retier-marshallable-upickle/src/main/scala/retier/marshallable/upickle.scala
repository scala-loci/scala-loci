package retier
package marshallable

import retier.transmission.AbstractionRef
import retier.transmission.DelegatingMarshallable
import retier.transmission.Marshallable
import scala.util.DynamicVariable
import scala.util.Try

object upickle extends _root_.upickle.AttributeTagged {
  def tagName = "$type"

  private val currentAbstraction =
    new DynamicVariable(Option.empty[(AbstractionRef, Int)])

  implicit def upickleBasedMarshallable[T: Reader: Writer] =
    new Marshallable[T] {
      def marshall(unmarshalled: T, abstraction: AbstractionRef) =
        currentAbstraction.withValue(Some((abstraction, 0))) {
          write(unmarshalled)
        }
      def unmarshall(marshalled: String, abstraction: AbstractionRef) = Try {
        currentAbstraction.withValue(Some((abstraction, 0))) {
          read[T](marshalled)
        }
      }
    }

  implicit def delegatingMarshallableBasedReader[T, U]
    (implicit
        marshallable: DelegatingMarshallable[T, U], reader: Reader[U]) =
    Reader[T] { expr =>
      currentAbstraction.value match {
        case Some((abstraction, index)) =>
          val derived = abstraction derive index.toString
          currentAbstraction.value = Some((abstraction, index + 1))
          marshallable unmarshall (reader read expr, derived)
        case _ =>
          throw new InvalidDynamicScope
      }
    }

  implicit def delegatingMarshallableBasedWriter[T, U]
    (implicit
        marshallable: DelegatingMarshallable[T, U], writer: Writer[U]) =
    Writer[T] { expr =>
      currentAbstraction.value match {
        case Some((abstraction, index)) =>
          val derived = abstraction derive index.toString
          currentAbstraction.value = Some((abstraction, index + 1))
          writer write (marshallable marshall (expr, derived))
        case _ =>
          throw new InvalidDynamicScope
      }
    }

  class InvalidDynamicScope extends RuntimeException(
    "Reader or writer for virtual composite marshallable object invoked " +
    "outside of a related marshallable object dynamic scope")
}
