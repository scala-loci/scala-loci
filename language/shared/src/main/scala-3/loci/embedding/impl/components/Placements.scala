package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental

@experimental
trait Placements:
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
      def modality(tpe: TypeRepr, peer: TypeRepr, of: Boolean): PlacementInfo = tpe match
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`embedding.of` && args.last =:= peer =>
          val info = modality(args.head, peer, of = true)
          if !of || !info.canonical then info else info.copy(canonical = false)
        case Refinement(parent, "on", TypeBounds(lo, hi)) if lo =:= peer && hi =:= peer =>
          val info = modality(parent, peer, of = true)
          if !info.canonical then info else info.copy(canonical = false)
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`language.Local` =>
          PlacementInfo(tpe, canonical = true, tpe, args.head, defn.NothingClass.typeRef, Modality.Local)
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`language.per` =>
          PlacementInfo(tpe, canonical = true, tpe, args.head, defn.NothingClass.typeRef, Modality.Subjective(args.last))
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.subjective =>
          PlacementInfo(tpe, canonical = false, symbols.`language.per`.typeRef.appliedTo(args.reverse), args.last, defn.NothingClass.typeRef, Modality.Subjective(args.head))
        case _ =>
          PlacementInfo(tpe, canonical = true, tpe, tpe, defn.NothingClass.typeRef, Modality.None)

      def placementInfo(tpe: TypeRepr): Option[PlacementInfo] = tpe match
        case AppliedType(tycon, List(value, peer)) if tycon.typeSymbol == symbols.`language.on` =>
          Some:
            val info = modality(value, peer, of = false)
            val canonicalType = symbols.`language.on`.typeRef.appliedTo(List(info.canonicalType, peer))
            info.copy(tpe = tpe, canonicalType = canonicalType, peerType = peer)
        case AppliedType(tycon, args @ List(value, peer)) if tycon.typeSymbol == symbols.`embedding.on` =>
          Some:
            val info = modality(value, peer, of = false)
            val canonicalType = symbols.`language.on`.typeRef.appliedTo(List(info.canonicalType, peer))
            info.copy(tpe = tpe, canonical = false, canonicalType = canonicalType, peerType = peer)
        case AndType(AppliedType(tycon, args), right) if tycon.typeSymbol == symbols.placed && args.last =:= right =>
          PlacementInfo(symbols.`embedding.on`.typeRef.appliedTo(args.reverse))
        case _ =>
          None

      tpe match
        case AppliedType(tycon, args @ List(AppliedType(_, List(peerType)), _)) if tycon.typeSymbol == symbols.contextFunction1 && args.head <:< types.context =>
          placementInfo(args.last) collect:
            case placementInfo if placementInfo.peerType =:= peerType && !placementInfo.canonical => placementInfo
        case tpe =>
          placementInfo(tpe)
    end apply
  end PlacementInfo

  object PlacedValueReference:
    def unapply(tree: Term): Option[(Term, PlacementInfo)] = tree match
      case Apply(Select(qualifier, names.apply), List(_)) if qualifier.symbol.exists && isMultitierModule(qualifier.symbol.owner) =>
        PlacementInfo(qualifier.tpe.widenTermRefByName.resultType) map { qualifier -> _ }
      case _ =>
        None
  end PlacedValueReference

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

  object PlacedAccess:
    def apply(term: Apply, arg: Term, typeApplies: List[TypeApply], apply: Apply, prefix: List[Term], transmission: Term, suffix: List[Term]) =
      Apply.copy(apply)(construct(Apply.copy(term)(term.fun, List(arg)), typeApplies), prefix ++ (transmission :: suffix))

    def unapply(term: Term) = term match
      case apply @ Apply(fun, args) =>
        val indices = clearTypeApplications(fun).tpe match
          case tpe @ MethodType(_, paramTypes, _) if tpe.isImplicit  =>
            paramTypes.zipWithIndex collect:
              case (tpe, index) if !(tpe =:= TypeRepr.of[Nothing]) && tpe <:< types.transmission => index
          case _ =>
            List.empty
        indices match
          case List(index) =>
            destruct(fun) match
              case (term @ Apply(fun, List(arg)), typeApplies)
                  if term.symbol.isImplicit || term.symbol.isExtensionMethod =>
                clearTypeApplications(fun).tpe match
                  case MethodType(_, List(paramType), _)
                    if !(paramType =:= TypeRepr.of[Nothing]) &&
                       !(paramType <:< types.`language.on`) &&
                       !(paramType <:< types.`embedding.on`) &&
                       (paramType <:< types.from || paramType <:< types.fromSingle || paramType <:< types.fromMultiple) =>
                    val (prefix, suffix) = args.splitAt(index)
                    Some(term, arg, typeApplies, apply, prefix, suffix.head, suffix.tail)
                  case _ =>
                    None
              case _ =>
                None
          case _ =>
            None
      case _ =>
        None

    private def destruct(term: Term): (Term, List[TypeApply]) = term match
      case typeApply @ TypeApply(fun, _) =>
        val (expr, typeApplies) = destruct(fun)
        (expr, typeApply :: typeApplies)
      case _ =>
        (term, List.empty)

    private def construct(term: Term, typeApplies: List[TypeApply]): Term = typeApplies match
      case typeApply :: typeApplies => TypeApply.copy(typeApply)(construct(term, typeApplies), typeApply.args)
      case _ => term
  end PlacedAccess
end Placements
