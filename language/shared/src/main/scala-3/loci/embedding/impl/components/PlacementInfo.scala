package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental

@experimental
trait PlacementInfo:
  this: Component & Commons =>
  import quotes.reflect.*

  enum Modality(val local: Boolean, val subjective: Boolean, val subjectivePeerType: Option[TypeRepr]):
    case None extends Modality(false, false, scala.None)
    case Local extends Modality(true, false, scala.None)
    case Subjective(peerType: TypeRepr) extends Modality(false, true, scala.Some(peerType))

  case class PlacementInfo(tpe: TypeRepr, canonical: Boolean, canonicalType: TypeRepr, valueType: TypeRepr, peerType: TypeRepr, modality: Modality):
    def showCanonical =
      val `subjective.safeShow` = modality.subjectivePeerType.fold(""): peerType =>
        s" ${symbols.`language.per`.name} ${peerType.safeShow(Printer.SafeTypeReprShortCode)}"
      s"${valueType.safeShow(Printer.SafeTypeReprShortCode)}${`subjective.safeShow`} ${symbols.`language.on`.name} ${peerType.safeShow(Printer.SafeTypeReprShortCode)}"

  object PlacementInfo:
    def apply(tpe: TypeRepr): Option[PlacementInfo] =
      def modality(tpe: TypeRepr) = tpe match
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`language.Local` =>
          Some(PlacementInfo(tpe, canonical = true, tpe, args.head, defn.NothingClass.typeRef, Modality.Local))
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`language.per` =>
          Some(PlacementInfo(tpe, canonical = true, tpe, args.head, defn.NothingClass.typeRef, Modality.Subjective(args.last)))
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.subjective =>
          Some(PlacementInfo(tpe, canonical = false, symbols.`language.per`.typeRef.appliedTo(args.reverse), args.last, defn.NothingClass.typeRef, Modality.Subjective(args.head)))
        case _ =>
          None

      tpe match
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`language.on` =>
          Some:
            modality(args.head).fold(PlacementInfo(tpe, canonical = true, tpe, args.head, args.last, Modality.None)): info =>
              val canonicalType = symbols.`language.on`.typeRef.appliedTo(List(info.canonicalType, args.last))
              info.copy(tpe = tpe, canonicalType = canonicalType, peerType = args.last)
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`embedding.on` =>
          Some:
            modality(args.head).fold(PlacementInfo(tpe, canonical = false, symbols.`language.on`.typeRef.appliedTo(args), args.head, args.last, Modality.None)): info =>
              val canonicalType = symbols.`language.on`.typeRef.appliedTo(List(info.canonicalType, args.last))
              info.copy(tpe = tpe, canonical = false, canonicalType = canonicalType, peerType = args.last)
        case _ =>
          None
    end apply
  end PlacementInfo

  object PlacedValue:
    def unapply(tree: Term): Option[(Term, PlacementInfo)] = tree match
      case Apply(Select(qualifier, names.apply), List(_)) if isMultitierModule(qualifier.symbol.owner) =>
        PlacementInfo(qualifier.tpe.widenTermRefByName.resultType) map { qualifier -> _ }
      case _ =>
        None
  end PlacedValue

  object PlacedStatement:
    def unapply(tree: Statement): Option[Statement] = tree match
      case tree: Term =>
        val tpe = tree.tpe
        Option.when(tpe.isContextFunctionType && tpe.contextFunctionResultType.typeSymbol == symbols.`embedding.on`)(tree)
      case tree: Definition =>
        val tpe = tree.symbol.info.resultType
        Option.when(tpe.isContextFunctionType && tpe.typeSymbol == symbols.`language.on`)(tree)
      case _ =>
        None
  end PlacedStatement
end PlacementInfo
