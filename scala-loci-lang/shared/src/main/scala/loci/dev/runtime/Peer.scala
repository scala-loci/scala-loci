package loci.dev
package runtime

import SignatureParser._

object Peer {
  type Tie = Tie.Value

  object Tie extends Enumeration {
    val Multiple, Optional, Single = Value

    def apply(signature: Peer.Signature, ties: Traversable[(Peer.Signature, Tie)]): Option[Tie] =
      ties.foldLeft(Option.empty[Int]) { case (multiplicity, (tieSignature, tieMultiplicity)) =>
        if (tieSignature == signature)
          multiplicity map { _ max tieMultiplicity.id } orElse Some(tieMultiplicity.id)
        else if (tieSignature <:< signature)
          multiplicity orElse Some(Tie.Multiple.id)
        else
          multiplicity
      } map { Tie(_) }
  }

  case class Base(name: String, module: Module.Signature)

  case class Signature(name: String, bases: List[Base], module: Module.Signature) {
    def <:<(signature: Signature): Boolean = {
      def subtype(name: String, module: Module.Signature) =
        name == signature.name && module <:< signature.module

      this == signature || subtype(name, module) || (bases exists { base => subtype(base.name, base.module) })
    }
  }

  object Signature {
    def create(name: String, bases: List[Signature], module: Module.Signature) =
      Signature(name, (bases flatMap { base => Base(base.name, base.module) :: base.bases }).distinct, module)

    def serialize(signature: Signature): String =
      elements(
        string(signature.name),
        list(signature.bases map Base.serialize),
        Module.Signature.serialize(signature.module))

    def deserialize(signature: String): Signature = {
      val Seq(name, bases, module) = SignatureParser(signature).asElements(3)
      Signature(
        name.asString,
        bases.asList map { base => Base.deserialize(base.asString) },
        Module.Signature.deserialize(module.asString))
    }
  }

  object Base {
    def serialize(base: Base): String =
      elements(
        string(base.name),
        Module.Signature.serialize(base.module))

    def deserialize(base: String): Base = {
      val Seq(name, module) = SignatureParser(base).asElements(2)
      Base(
        name.asString,
        Module.Signature.deserialize(module.asString))
    }
  }
}
