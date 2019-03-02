package loci.dev
package runtime

import SignatureParser._

object Module {
  case class Base(name: String, path: List[(String, Signature)])

  case class Signature(name: String, bases: List[Base], path: List[(String, Signature)]) {
    def <:<(signature: Signature): Boolean = {
      def subtype(name: String, path: List[(String, Signature)]) =
        name == signature.name &&
        path.size >= signature.path.size &&
        (signature.path.isEmpty || {
          val zip = path zip signature.path
          val ((_, thisSignature), (_, thatSignature)) = zip.last

          (zip forall { case ((thisValue, _), (thatValue, _)) => thisValue == thatValue }) &&
          thisSignature <:< thatSignature
        })

      this == signature || subtype(name, path) || (bases exists { base => subtype(base.name, path ++ base.path) })
    }
  }

  object Signature {
    def create(name: String, bases: List[Signature], outer: Signature, value: String) =
      Signature(name, (bases flatMap { base => Base(base.name, base.path) :: base.bases }).distinct, (value -> outer) :: outer.path)

    def create(name: String, bases: List[Signature]) =
      Signature(name, (bases flatMap { base => Base(base.name, base.path) :: base.bases }).distinct, List.empty)

    def create(self: Signature, outer: Signature, value: String) =
      Signature(self.name, self.bases, (value -> outer) :: outer.path)

    def serialize(signature: Signature): String =
      elements(
        string(signature.name),
        list(signature.bases map Base.serialize),
        list(signature.path map { case (name, signature) =>
          elements(name, serialize(signature))
        }))

    def deserialize(signature: String): Signature = {
      val Seq(name, bases, path) = SignatureParser(signature).asElements(3)
      Signature(
        name.asString,
        bases.asList map { base => Base.deserialize(base.asString) },
        path.asList map { element =>
          val Seq(name, signature) = element.asElements(2)
          name.asString -> deserialize(signature.asString)
        })
    }
  }

  object Base {
    def serialize(base: Base): String =
      elements(
        string(base.name),
        list(base.path map { case (name, signature) =>
          elements(name, Signature.serialize(signature))
        }))

    def deserialize(base: String): Base = {
      val Seq(name, path) = SignatureParser(base).asElements(2)
      Base(
        name.asString,
        path.asList map { element =>
          val Seq(name, signature) = element.asElements(2)
          name.asString -> Signature.deserialize(signature.asString)
        })
    }
  }
}
