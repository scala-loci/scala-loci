package loci
package runtime

import transmitter.Parser._

import scala.util.Try

object Peer {
  sealed trait Tie extends Ordered[Tie] {
    val key: Int
    def compare(that: Tie) = key - that.key
  }

  object Tie {
    case object Multiple extends Tie { val key = 0 }
    case object Optional extends Tie { val key = 1 }
    case object Single extends Tie { val key = 2 }

    def apply(key: Int): Tie = key match {
      case 0 => Multiple
      case 1 => Optional
      case 2 => Single
      case _ => throw new NoSuchElementException(s"key not found: $key")
    }

    def apply(signature: Peer.Signature, ties: compatibility.Iterable[(Peer.Signature, Tie)]): Option[Tie] =
      ties.foldLeft(Option.empty[Int]) { case (multiplicity, (tieSignature, tieMultiplicity)) =>
        if (tieSignature == signature)
          multiplicity map { _ max tieMultiplicity.key } orElse Some(tieMultiplicity.key)
        else if (tieSignature < signature)
          multiplicity orElse Some(Tie.Multiple.key)
        else
          multiplicity
      } map { Tie(_) }
  }

  case class Signature(name: String, parents: List[Signature], module: Module.Signature)
      extends PartiallyOrdered[Signature] {
    if (parents exists { _.module.name != module.name })
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

    override def toString: String =
      if (parents.isEmpty)
        s"$module.$name"
      else
        s"$module.$name<:{${parents mkString ","}}"
  }

  object Signature {
    private def serializeBases(signatures: List[Signature]): Serializer =
      list(signatures map { signature =>
        elements(
          string(signature.name),
          list(signature.module.path map string),
          serializeBases(signature.parents))
      })

    def serialize(signature: Signature): String =
      elements(
        string(signature.name),
        list(signature.module.path map string),
        serializeBases(signature.parents),
        string(signature.module.name)).toString

    private def deserializeBases(signatures: Deserializer, moduleName: String): List[Signature] =
      signatures.asList map { signature =>
        val Seq(name, path, bases) = signature.asElements(3): @unchecked
        Signature(
          name.asString,
          deserializeBases(bases, moduleName),
          Module.Signature(moduleName, path.asList map { _.asString }))
      }

    def deserialize(signature: String): Try[Signature] = Try {
      val Seq(name, path, bases, module) = parse(signature).asElements(4): @unchecked
      val moduleName = module.asString
      Signature(
        name.asString,
        deserializeBases(bases, moduleName),
        Module.Signature(moduleName, path.asList map { _.asString }))
    }
  }
}
