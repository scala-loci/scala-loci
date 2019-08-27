package loci
package runtime

import transmitter.Parser._

import scala.util.Try

object Module {
  case class Signature(name: String, path: List[String])

  object Signature {
    def apply(outer: Signature, value: String): Signature =
      Signature(outer.name, outer.path :+ value)

    def apply(name: String): Signature =
      Signature(name, List.empty)

    def serialize(signature: Signature): String =
      elements(
        string(signature.name),
        list(signature.path map string)).toString

    def deserialize(signature: String): Try[Signature] = Try {
      val Seq(name, path) = parse(signature).asElements(2)
      Signature(
        name.asString,
        path.asList map { _.asString })
    }
  }
}
