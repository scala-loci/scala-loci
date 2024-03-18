package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental
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
        case _ if impl.owner != synthesizedPlacedValues(module, peer).symbol =>
          Literal(NullConstant()).select(symbols.asInstanceOf).appliedToType(impl.info.resultType.substituteParamRefsByTermRefs(impl))
        case PlacedStatement(_) =>
          extractPlacedBody(rhs).changeOwner(impl)
        case _ =>
          rhs.changeOwner(impl)

    if impl.isMethod then
      DefDef(impl, paramss => synthesizedBody map:
        _.substituteRefs((original.symbol.paramSymss.flatten zip (paramss flatMap { _ map { _.symbol } })).toMap, impl))
    else
      ValDef(impl, synthesizedBody)
  end synthesizePlacedDefinition

  def split(module: ClassDef): ClassDef =
    val indices = mutable.Map.empty[Symbol, Int]

    val universalPlacedValues = synthesizedPlacedValues(module.symbol, defn.AnyClass).symbol
    val universalPlacedValuesLocalDummy = SymbolMutator.getOrErrorAndAbort.createLocalDummy(universalPlacedValues)

    extension (self: Map[Symbol, List[Statement]])
      inline def prepended(symbol: Symbol)(stats: Statement*) =
        self + (symbol -> (stats.toList ++ self.getOrElse(symbol, List.empty)))

    val placedBodies = module.body.foldLeft(Map.empty[Symbol, List[Statement]]):
      case (bodies, stat @ (_: ValDef | _: DefDef)) if (stat.symbol.isField || stat.symbol.isMethod) && !stat.symbol.isModuleDef =>
        val peer = PlacementInfo(stat.symbol.info.widenTermRefByName.resultType).fold(defn.AnyClass) { _.peerType.typeSymbol }
        val definitions = synthesizedDefinitions(stat.symbol)

        val bodiesWithBinding = definitions match
          case SynthesizedDefinitions(_, binding, None, _) =>
            bodies.prepended(binding.owner)(synthesizePlacedDefinition(binding, stat, module.symbol, peer))
          case SynthesizedDefinitions(_, binding, Some(init), _) =>
            bodies.prepended(binding.owner)(ValDef(binding, Some(Ref(init).appliedToNone)))
              .prepended(init.owner)(synthesizePlacedDefinition(init, stat, module.symbol, peer))
          case _ =>
            bodies

        definitions.impls.foldLeft(bodiesWithBinding): (bodies, impl) =>
          bodies.prepended(impl.owner)(synthesizePlacedDefinition(impl, stat, module.symbol, peer))

      case (bodies, term: ClassDef) if term.symbol.isModuleDef =>
        val placedValues = synthesizedPlacedValues(term.symbol, defn.AnyClass).symbol
        val nestedModule = synthesizedDefinitions(term.symbol)
        val parents = List[TypeTree | Term](
          TypeTree.of[Object],
          New(TypeIdent(placedValues)).select(placedValues.primaryConstructor).appliedTo(This(nestedModule.binding.owner)))
        def bodySymbols(symbol: Symbol): List[Symbol] =
          if parents exists { _.tpe <:< types.placedValues } then
            List(newMethod(symbol, names.systemCreate, ByNameType(types.system), Flags.Protected, Symbol.noSymbol))
          else
            List.empty

        val symbol = newClass(nestedModule.binding, names.anon, Flags.Final | Flags.NoInits | Flags.Synthetic, parents map { _.tpe }, bodySymbols, selfType = None)

        val body = symbol.declarations collect:
          case symbol if !symbol.isClassConstructor =>
            DefDef(symbol, _ => Some(Select.unique(This(nestedModule.binding.owner), names.system)))

        val classDef = ClassDef(symbol, parents, body)
        val instantiation = Typed(New(TypeIdent(symbol)).select(symbol.primaryConstructor).appliedToNone, TypeIdent(placedValues))

        bodies.prepended(nestedModule.binding.owner)(ValDef(nestedModule.binding, Some(Block(List(classDef), instantiation))))

      case (bodies, term: Term) =>
        val placementInfo = PlacementInfo(term.tpe.resultType)
        val peer = placementInfo.fold(defn.AnyClass) { _.peerType.typeSymbol }
        val index = indices.getOrElse(peer, 0)
        indices += peer -> (index + 1)

        val bodiesUniversalValues =
          if !placementInfo.isDefined then
            bodies.prepended(universalPlacedValues)(term.changeOwner(universalPlacedValuesLocalDummy))
          else if peer == defn.AnyClass then
            bodies.prepended(universalPlacedValues)(extractPlacedBody(term).changeOwner(universalPlacedValuesLocalDummy))
          else
            synthesizedStatement(module.symbol, peer, index).fold(bodies): statement =>
              bodies.prepended(universalPlacedValues)(DefDef(statement.binding, _ => Some(Literal(UnitConstant()))), Ref(statement.binding).appliedToNone)

        if peer == defn.AnyClass then
          bodiesUniversalValues
        else
          synthesizedStatement(module.symbol, peer, index).fold(bodiesUniversalValues): statement =>
            val rhs = extractPlacedBody(term) match
              case Block(statements, expr) if expr.tpe.typeSymbol != defn.UnitClass =>
                Block(statements :+ expr, Literal(UnitConstant()))
              case expr if expr.tpe.typeSymbol != defn.UnitClass =>
                Block(List(expr), Literal(UnitConstant()))
              case expr =>
                expr
            statement.impls.foldLeft(bodiesUniversalValues): (bodies, impl) =>
              bodies.prepended(impl.owner)(DefDef(impl, _ => Some(rhs.changeOwner(impl))))

      case (bodies, _) =>
        bodies
    end placedBodies

    val placedBody = PeerInfo.ofModule(module.symbol) map: peerInfo =>
      val SynthesizedPlacedValues(placedValues, parents) = synthesizedPlacedValues(module.symbol, peerInfo.peerType.typeSymbol)
      val tpe = placedValues.typeRef
      val params = placedValues.declaredFields collect { case symbol if symbol.isParamAccessor => ValDef(symbol, None) }
      val classDef = ClassDef(placedValues, parents map { parent => TypeTree.of(using parent.asType) }, params ++ placedBodies.getOrElse(placedValues, List.empty).reverse)
      ClassDef.copy(classDef)(classDef.name, DefDef(classDef.constructor.symbol, _ => None), classDef.parents, classDef.self, classDef.body)

    def eraseBody(stat: Definition, term: Term) =
      transformBody(term, stat.symbol): (expr, owners) =>
        val tpe = owners.foldLeft(expr.tpe) { _.substituteParamRefsByTermRefs(_) }
        Literal(NullConstant()).select(symbols.asInstanceOf).appliedToType(tpe)

    val body = module.body flatMap:
      case stat @ ValDef(name, tpt, rhs) if !stat.symbol.isModuleDef =>
        Some(ValDef.copy(stat)(name, tpt, rhs map { eraseBody(stat, _) }))
      case stat @ DefDef(name, paramss, tpt, rhs) if !stat.symbol.isFieldAccessor =>
        Some(DefDef.copy(stat)(name, paramss, tpt, rhs map { eraseBody(stat, _) }))
      case stat: ClassDef if stat.symbol.isModuleDef && !isMultitierModule(stat.symbol) =>
        Some(split(stat))
      case stat: Definition =>
        Some(stat)
      case stat =>
        None

    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body ++ placedBody)
  end split
end Splitting

