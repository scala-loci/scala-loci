package retier
package transmission

import scala.util.Try

sealed trait Marshallable[T] {
  def marshall(unmarshalled: T, abstraction: AbstractionRef): String
  def unmarshall(marshalled: String, abstraction: AbstractionRef): Try[T]
}

trait PushBasedMarshallable[T] extends Marshallable[T]

trait PullBasedMarshallable[T] extends Marshallable[T]
