package loci
package embedding
package impl
package components

import embedding.*
import utility.reflectionExtensions.*

import scala.quoted.*

trait PlacementInfo:
  this: Component with Commons =>
  import quotes.reflect.*

  enum Modality(val local: Boolean, val subjective: Boolean, val subjectivePeerType: Option[TypeRepr]):
    case None extends Modality(false, false, scala.None)
    case Local extends Modality(true, false, scala.None)
    case Subjective(peerType: TypeRepr) extends Modality(false, true, scala.Some(peerType))

  case class PlacementInfo(tpe: TypeRepr, canonical: Boolean, canonicalType: TypeRepr, valueType: TypeRepr, peerType: TypeRepr, modality: Modality):
    def showCanonical =
      given Printer[TypeRepr] = Printer.TypeReprShortCode
      val `subjective.show` = modality.subjectivePeerType.fold("") { peerType => s" ${symbols.`language.per`.name} ${peerType.show}" }
      s"${valueType.show}${`subjective.show`} ${symbols.`language.on`.name} ${peerType.show}"

  object PlacementInfo:
    def apply(tpe: TypeRepr, acceptUnliftedSubjectiveFunction: Boolean = false): Option[PlacementInfo] =
      def modality(tpe: TypeRepr) = tpe match
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`language.Local` =>
          Some(PlacementInfo(tpe, canonical = true, tpe, args.head, defn.NothingClass.typeRef, Modality.Local))
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`language.per` =>
          Some(PlacementInfo(tpe, canonical = true, tpe, args.head, defn.NothingClass.typeRef, Modality.Subjective(args.last)))
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.subjective =>
          Some(PlacementInfo(tpe, canonical = false, symbols.`language.per`.typeRef.appliedTo(args.reverse), args.last, defn.NothingClass.typeRef, Modality.Subjective(args.head)))
        case _ =>
          None

      def placement(tpe: TypeRepr) = tpe match
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`language.on` =>
          Some:
            modality(args.head).fold(PlacementInfo(tpe, canonical = true, tpe, args.head, args.last, Modality.None)): info =>
              val canonicalType = symbols.`language.on`.typeRef.appliedTo(List(info.canonicalType, args.last))
              info.copy(tpe = tpe, canonicalType = canonicalType, peerType = args.last)
        case AppliedType(tycon, args)
          if tycon.typeSymbol == symbols.`embedding.on` ||
             tycon.typeSymbol == symbols.`Placed.on` ||
             tycon.typeSymbol == symbols.`Placed.Subjective.on` =>
          Some:
            modality(args.head).fold(PlacementInfo(tpe, canonical = false, symbols.`language.on`.typeRef.appliedTo(args), args.head, args.last, Modality.None)): info =>
              val canonicalType = symbols.`language.on`.typeRef.appliedTo(List(info.canonicalType, args.last))
              info.copy(tpe = tpe, canonical = false, canonicalType = canonicalType, peerType = args.last)
        case _ =>
          None

      tpe match
        case AppliedType(tycon, List(remote, _))
            if acceptUnliftedSubjectiveFunction && tycon.typeSymbol == symbols.function1 && remote <:< types.remote =>
          placement(tpe) collect:
            case info if !info.modality.subjective =>
              val subjective = remote.widenDealias.typeArgs.head
              val valueType = symbols.`language.per`.typeRef.appliedTo(List(info.valueType, subjective))
              val canonicalType = symbols.`language.on`.typeRef.appliedTo(List(valueType, info.peerType))
              info.copy(tpe = tpe, canonical = false, canonicalType = canonicalType, valueType = valueType, modality = Modality.Subjective(subjective))
        case _ =>
          placement(tpe)
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
