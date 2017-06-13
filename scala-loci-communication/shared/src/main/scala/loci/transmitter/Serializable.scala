package loci
package transmitter

import scala.util.Try
import scala.annotation.implicitNotFound

@implicitNotFound("${T} is not serializable")
trait Serializable[T] {
  def serialize(value: T): MessageBuffer
  def deserialize(value: MessageBuffer): Try[T]
}
