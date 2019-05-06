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
        else if (tieSignature < signature)
          multiplicity orElse Some(Tie.Multiple.id)
        else
          multiplicity
      } map { Tie(_) }
  }

  case class Signature(name: String, parents: List[Signature], module: Module.Signature)
      extends PartiallyOrdered[Signature] {
    if (parents exists { _.module.name != module.name})
      throw new IllegalArgumentException("Base peer signature for different multitier module")

    def bases: Set[Signature] =
      parents.toSet ++ (parents flatMap { _.bases }) + this

    def tryCompareTo[S >: Signature](other: S)(implicit ev: S => PartiallyOrdered[S]): Option[Int] =
      other match {
        case other: Signature if other canEqual this =>
          def contains(bases: List[Signature], signature: Signature): Boolean =
            bases exists { base =>
              base.name == signature.name && base.module == signature.module ||
              contains(base.parents, signature)
            }

          if (name == other.name && module == other.module) Some(0)
          else if (contains(parents, other)) Some(-1)
          else if (contains(other.parents, this)) Some(1)
          else None

        case _ =>
          None
      }
  }

  object Signature {
    private def serializeBases(signatures: List[Signature]): String =
      list(signatures map { signature =>
        elements(
          string(signature.name),
          list(signature.module.path),
          serializeBases(signature.parents))
      })

    def serialize(signature: Signature): String =
      elements(
        string(signature.name),
        list(signature.module.path),
        serializeBases(signature.parents),
        string(signature.module.name))

    private def deserializeBases(signatures: SignatureParser, moduleName: String): List[Signature] =
      signatures.asList map { signature =>
        val Seq(name, path, bases) = signature.asElements(3)
        Signature(
          name.asString,
          deserializeBases(bases, moduleName),
          Module.Signature(moduleName, path.asList map { _.asString }))
      }

    def deserialize(signature: String): Signature = {
      val Seq(name, path, bases, module) = SignatureParser(signature).asElements(4)
      val moduleName = module.asString
      Signature(
        name.asString,
        deserializeBases(bases, moduleName),
        Module.Signature(moduleName, path.asList map { _.asString }))
    }
  }
}
