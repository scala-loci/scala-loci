package loci.dev
package runtime

import SignatureParser._

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
        list(signature.path))

    def deserialize(signature: String): Signature = {
      val Seq(name, path) = SignatureParser(signature).asElements(2)
      Signature(
        name.asString,
        path.asList map { _.asString })
    }
  }
}
