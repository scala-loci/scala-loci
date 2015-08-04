package retier
package transmission

import scala.util.Try

trait Marshallable[T] {
  def marshall(unmarshalled: T, abstraction: AbstractionRef): String
  def unmarshall(marshalled: String, abstraction: AbstractionRef): Try[T]
}

trait DelegatingMarshallable[T, U] {
  def marshall(composed: T, abstraction: AbstractionRef): U
  def unmarshall(decomposed: U, abstraction: AbstractionRef): T
}
