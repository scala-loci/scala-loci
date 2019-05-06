package loci.dev
package runtime

import loci.MessageBuffer

import scala.annotation.compileTimeOnly
import scala.util.{Failure, Try}

trait PlacedValues {
  final val $loci$sys: System = $loci$sys$create
  protected def $loci$sys$create: System

  def $loci$dispatch(
      request: MessageBuffer,
      signature: Value.Signature,
      abstraction: AbstractionRef): Try[MessageBuffer] =
    Failure(new IllegalAccessException(
      s"Request for ${Value.Signature.serialize(signature)} could not be dispatched"))
}

object PlacedValues {
  @compileTimeOnly("Multitier type can only be instantiated in module in which it is defined")
  implicit def placedValues: Nothing =
    throw new NotImplementedError("Multitier type can only be instantiated in module in which it is defined")
}
