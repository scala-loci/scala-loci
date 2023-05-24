package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.{experimental, targetName}

@experimental
trait PeerInfo:
  this: Component & Commons =>
  import quotes.reflect.*

  enum Multiplicity:
    case Single
    case Optional
    case Multiple

  case class PeerInfo(peerType: TypeRepr, ties: List[(TypeRepr, Multiplicity)])

  object PeerInfo:
    def apply(tpe: TypeRepr): Option[PeerInfo] = tpe match
      case tpe: TypeRef if tpe.typeSymbol.hasAnnotation(symbols.peer) => tpe.qualifier.memberType(tpe.typeSymbol) match
        case TypeBounds(low: TypeRef, hi) if bottomType(low) => hi match
          case hi: TypeRef if topType(hi) => Some(PeerInfo(tpe, List.empty))
          case Refinement(parent: TypeRef, names.tie, info) if topType(parent) => info match
            case TypeBounds(low: TypeRef, hi) if bottomType(low) => ties(hi) map { PeerInfo(tpe, _) }
            case _ => None
          case _ => None
        case _ => None
      case _ => None

    @targetName("ofModuleSymbol")
    def ofModule(symbol: Symbol): List[PeerInfo] =
      ofModule(This(symbol).tpe)

    @targetName("ofModuleType")
    def ofModule(tpe: TypeRepr): List[PeerInfo] =
      val symbol =
        if tpe.typeSymbol.exists then tpe.typeSymbol
        else tpe.baseClasses.headOption getOrElse Symbol.noSymbol
      symbol.typeMembers flatMap { symbol => PeerInfo(tpe.select(symbol)) }

    private def ties(tpe: TypeRepr): Option[List[(TypeRepr, Multiplicity)]] = tpe match
      case tpe: TypeRef if topType(tpe) =>
        Some(List.empty)
      case AndType(left, right) =>
        ties(left) flatMap { left => ties(right) map { left ++ _ } }
      case AppliedType(tycon, List(arg)) =>
        val symbol = tycon.typeSymbol
        val multiplicity =
          if symbol == symbols.single then Some(Multiplicity.Single)
          else if symbol == symbols.optional then Some(Multiplicity.Optional)
          else if symbol == symbols.multiple then Some(Multiplicity.Multiple)
          else None
        multiplicity map { multiplicity => List(arg.stripLazyRef -> multiplicity) }
      case _ =>
        None

    private def topType(tpe: TypeRef) =
      val symbol = tpe.typeSymbol
      symbol == defn.AnyClass || symbol == defn.AnyRefClass || symbol == defn.ObjectClass

    private def bottomType(tpe: TypeRef) =
      tpe.typeSymbol == defn.NothingClass
  end PeerInfo
end PeerInfo
