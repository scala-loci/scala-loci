package loci
package transmission

import scala.util.Try
import scala.annotation.implicitNotFound

@implicitNotFound("${T} is not serializable")
trait Serializable[T] {
  def serialize(value: T): String
  def deserialize(value: String): Try[T]
}
