package loci
package runtime

import scala.util.{Failure, Try}

trait PlacedValues(final val $loci$sys: System):
  def $loci$dispatch(
      request: MessageBuffer,
      signature: Value.Signature,
      path: List[String],
      reference: Value.Reference): Try[MessageBuffer] =
    Failure(new transmitter.RemoteAccessException(
      s"request for ${Value.Signature.serialize(signature)} could not be dispatched"))
