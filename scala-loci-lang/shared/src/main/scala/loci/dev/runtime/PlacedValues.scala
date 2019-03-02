package loci.dev
package runtime

import loci.MessageBuffer

import scala.annotation.compileTimeOnly
import scala.util.{Failure, Try}

trait PlacedValues {
  def $loci$sys: System

  def $loci$dispatch(
      request: MessageBuffer,
      signature: String,
      abstraction: AbstractionRef): Try[MessageBuffer] =
    Failure(new Exception(s"Request for $signature could not be dispatched"))

  $loci$sys.setup(this)
}

object PlacedValues {
  @compileTimeOnly("Multitier type can only be instantiated in module in which it is defined")
  implicit def placedValues: Nothing =
    throw new NotImplementedError("Multitier type can only be instantiated in module in which it is defined")
}
