package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.{experimental, threadUnsafe}
import scala.collection.mutable

@experimental
trait Splitting:
  this: Component & Commons & Placements & Peers & PlacedTransformations & PlacedStatements & Synthesis =>
  import quotes.reflect.*

  private def synthesizePlacedDefinition(impl: Symbol, original: Statement, module: Symbol, peer: Symbol): ValDef | DefDef =
    val rhs = original match
      case stat: ValDef => stat.rhs
      case stat: DefDef => stat.rhs
      case _ => None

    val synthesizedBody = rhs map: rhs =>
      original match
        case _ if impl.owner != placedValuesSymbol(module, peer) =>
          Literal(NullConstant()).select(symbols.asInstanceOf).appliedToType(impl.info.resultType.substituteParamRefsByTermRefs(impl))
        case PlacedStatement(_) =>
          extractPlacedBody(rhs).changeOwner(impl)
        case _ =>
          rhs.changeOwner(impl)

    if impl.isMethod then
      DefDef(impl, paramss => synthesizedBody map:
        _.substituteRefs((original.symbol.paramSymss.flatten zip (paramss flatMap { _ map { _.symbol } })).toMap, original.symbol))
    else
      ValDef(impl, synthesizedBody)
  end synthesizePlacedDefinition

  def split(module: ClassDef): ClassDef =
    val indices = mutable.Map.empty[Symbol, Int]

    val unaryProcedureType = MethodType(List.empty)(_ => List.empty, _ => TypeRepr.of[Unit])
    val universalPlacedValues = placedValuesSymbol(module.symbol, defn.AnyClass)
    val universalPlacedValuesLocalDummy = SymbolMutator.getOrErrorAndAbort.createLocalDummy(universalPlacedValues)

    extension (self: Map[Symbol, List[Statement]])
      inline def prepended(symbol: Symbol, stats: Statement*) =
        self + (symbol -> (stats.toList ++ self.getOrElse(symbol, List.empty)))

    val placedBodies = module.body.foldLeft(Map.empty[Symbol, List[Statement]]):
      case (bodies, stat @ (_: ValDef | _: DefDef)) if !stat.symbol.isModuleDef =>
        val peer = PlacementInfo(stat.symbol.info.widenTermRefByName.resultType).fold(defn.AnyClass) { _.peerType.typeSymbol }
        val definitions = synthesizedDefinitions(stat.symbol)

        val bodiesWithBinding = definitions match
          case SynthesizedDefinitions(_, Some(binding), List()) =>
            bodies.prepended(binding.owner, synthesizePlacedDefinition(binding, stat, module.symbol, peer))
          case SynthesizedDefinitions(_, Some(binding), impl :: _) =>
            bodies.prepended(binding.owner, ValDef(binding, Some(Ref(impl).appliedToNone)))
          case _ =>
            bodies

        definitions.impls.foldLeft(bodiesWithBinding): (bodies, impl) =>
          bodies.prepended(impl.owner, synthesizePlacedDefinition(impl, stat, module.symbol, peer))

      case (bodies, term: Term) =>
        val placementInfo = PlacementInfo(term.tpe.resultType)
        val peer = placementInfo.fold(defn.AnyClass) { _.peerType.typeSymbol }
        val index = indices.getOrElse(peer, 0)
        val name = s"<placed statement ${index} of ${fullName(peer)}>"
        indices += peer -> (index + 1)

        val bodiesUniversalValues =
          if !placementInfo.isDefined then
            bodies.prepended(universalPlacedValues, term.changeOwner(universalPlacedValuesLocalDummy))
          else if peer == defn.AnyClass then
            bodies.prepended(universalPlacedValues, extractPlacedBody(term).changeOwner(universalPlacedValuesLocalDummy))
          else
            val symbol = newMethod(universalPlacedValues, name, unaryProcedureType, Flags.Synthetic, Symbol.noSymbol)
            bodies.prepended(universalPlacedValues, DefDef(symbol, _ => Some(Literal(UnitConstant()))), Ref(symbol).appliedToNone)

        if peer == defn.AnyClass then
          bodiesUniversalValues
        else
          val placedValues = placedValuesSymbol(module.symbol, peer)
          val symbol = newMethod(placedValues, name, unaryProcedureType, Flags.Synthetic | Flags.Override, Symbol.noSymbol)
          val rhs = extractPlacedBody(term) match
            case Block(statements, expr) if expr.tpe.typeSymbol != defn.UnitClass =>
              Block(statements :+ expr, Literal(UnitConstant()))
            case expr if expr.tpe.typeSymbol != defn.UnitClass =>
              Block(List(expr), Literal(UnitConstant()))
            case expr =>
              expr
          bodiesUniversalValues.prepended(placedValues, DefDef(symbol, _ => Some(rhs.changeOwner(symbol))))

      case (bodies, _) =>
        bodies
    end placedBodies

    val placedBody = PeerInfo.ofModule(module.symbol) map: peerInfo =>
      val placedValues = placedValuesSymbol(module.symbol, peerInfo.peerType.typeSymbol)
      val tpe = placedValues.typeRef
      val parents = tpe.baseClasses.tail map { parent => TypeTree.of(using tpe.baseType(parent).asType) }
      ClassDef(placedValues, parents, placedBodies.getOrElse(placedValues, List.empty).reverse)

    def eraseBody(stat: Definition, term: Term) =
      transformBody(term, stat.symbol): (expr, owners) =>
        val tpe = owners.foldLeft(expr.tpe) { _.substituteParamRefsByTermRefs(_) }
        Literal(NullConstant()).select(symbols.asInstanceOf).appliedToType(tpe)

    val body = module.body flatMap:
      case stat @ ValDef(name, tpt, rhs) if !stat.symbol.isModuleDef =>
        Some(ValDef.copy(stat)(name, tpt, rhs map { eraseBody(stat, _) }))
      case stat @ DefDef(name, paramss, tpt, rhs) if !stat.symbol.isFieldAccessor =>
        Some(DefDef.copy(stat)(name, paramss, tpt, rhs map { eraseBody(stat, _) }))
      case stat: Definition =>
        Some(stat)
      case stat =>
        None

    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body ++ placedBody)
  end split
end Splitting

