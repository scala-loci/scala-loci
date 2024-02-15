package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.{experimental, targetName}

@experimental
trait Peers:
  this: Component & Commons =>
  import quotes.reflect.*

  enum Multiplicity:
    case Single
    case Optional
    case Multiple

  case class PeerInfo(peerType: TypeRepr, parents: List[TypeRepr], ties: List[(TypeRepr, Multiplicity)])

  object PeerInfo:
    def apply(tpe: TypeRepr): Option[PeerInfo] =
      check(tpe, Position.ofMacroExpansion).toOption

    def check(tpe: TypeRepr, pos: Position): Either[(String, Position), PeerInfo] = tpe match
      case tpe: TypeRef if tpe.typeSymbol == defn.AnyClass =>
        Right(PeerInfo(tpe, List.empty, List.empty))
      case tpe: TypeRef =>
        val symbol = tpe.typeSymbol
        val position = symbol.pos getOrElse pos
        if tpe.typeSymbol.hasAnnotation(symbols.peer) then
          tpe.qualifier.memberType(symbol) match
            case TypeBounds(low: TypeRef, hi) =>
              if bottomType(low) then
                parentsAndTies(hi, pos) map { (parents, ties) => PeerInfo(tpe, parents, ties) }
              else
                Left(s"Lower type bound not allowed in peer specification: >: ${TypeBounds.lower(low).safeShow(Printer.SafeTypeReprShortCode)}", position)
            case tpe =>
              Left(s"Unexpected type in peer specification: ${tpe.safeShow(Printer.SafeTypeReprShortCode)}", position)
        else
          Left(s"No peer type: @peer type ${symbol.name}", position)
      case _ =>
        Left("No peer type: @peer type", pos)

    @targetName("ofModuleSymbol")
    def ofModule(symbol: Symbol): List[PeerInfo] =
      ofModule(This(symbol).tpe)

    @targetName("ofModuleType")
    def ofModule(tpe: TypeRepr): List[PeerInfo] =
      val symbol = tpe.typeSymbol orElse (tpe.baseClasses.headOption getOrElse Symbol.noSymbol)
      PeerInfo(defn.AnyClass.typeRef, List.empty, List.empty) :: (symbol.typeMembers flatMap { symbol => PeerInfo(tpe.select(symbol)) })

    private def parentsAndTies(tpe: TypeRepr, pos: Position): Either[(String, Position), (List[TypeRepr], List[(TypeRepr, Multiplicity)])] = tpe match
      case tpe: TypeRef =>
        Right((if topType(tpe) then List.empty else List(tpe), List.empty))
      case AndType(left, right) =>
        parentsAndTies(left, pos) flatMap: (leftParents, leftTies) =>
          parentsAndTies(right, pos) map: (rightParents, rightTies) =>
            (leftParents ++ rightParents, leftTies ++ rightTies)
      case OrType(_, _) =>
        Left(s"Union type not allowed in peer specification: ${tpe.safeShow(Printer.SafeTypeReprShortCode)}", pos)
      case Refinement(parent: TypeRef, names.tie, info) => info match
        case TypeBounds(low: TypeRef, hi) =>
          if bottomType(low) then
            ties(hi, pos) flatMap: ties =>
              parentsAndTies(parent, pos) map: (parentParents, parentTies) =>
                (parentParents, parentTies ++ ties)
          else
            Left(s"Lower type bound not allowed for peer specification: >: ${TypeBounds.lower(low).safeShow(Printer.SafeTypeReprShortCode)}", pos)
        case _ =>
          Left(s"Unexpected type in peer specification: ${info.safeShow(Printer.SafeTypeReprShortCode)}", pos)

    private def ties(tpe: TypeRepr, pos: Position): Either[(String, Position), List[(TypeRepr, Multiplicity)]] = tpe match
      case tpe: TypeRef if topType(tpe) =>
        Right(List.empty)
      case AndType(left, right) =>
        ties(left, pos) flatMap { left => ties(right, pos) map { left ++ _ } }
      case OrType(_, _) =>
        Left(s"Union type not allowed in peer tie specification: ${tpe.safeShow(Printer.SafeTypeReprShortCode)}", pos)
      case AppliedType(tycon, List(arg)) =>
        val symbol = tycon.typeSymbol
        val multiplicity =
          if symbol == symbols.single then Right(Multiplicity.Single)
          else if symbol == symbols.optional then Right(Multiplicity.Optional)
          else if symbol == symbols.multiple then Right(Multiplicity.Multiple)
          else Left(
            s"Unexpected multiplicity in peer tie specification: ${symbol.name} " +
            s"(expected one of: ${symbols.single.name}, ${symbols.optional.name}, ${symbols.multiple.name}",
            pos)
        multiplicity map { multiplicity => List(arg.stripLazyRef -> multiplicity) }
      case _ =>
        Left(s"Unexpected type in peer tie specification: ${tpe.safeShow(Printer.SafeTypeReprShortCode)}", pos)

    private def topType(tpe: TypeRef) =
      val symbol = tpe.typeSymbol
      symbol == defn.AnyClass || symbol == defn.AnyRefClass || symbol == defn.ObjectClass

    private def bottomType(tpe: TypeRef) =
      tpe.typeSymbol == defn.NothingClass
  end PeerInfo
end Peers
