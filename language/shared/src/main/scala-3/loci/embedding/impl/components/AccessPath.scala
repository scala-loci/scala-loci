package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental

@experimental
trait AccessPath:
  this: Component & Commons & PlacedValueSynthesis =>
  import quotes.reflect.*

  private def multitierOuterAccess(from: Symbol, to: Symbol, peer: Symbol) =
    def multitierOuterAccess(symbol: Symbol, path: Term): Option[Term] =
      if symbol == to then
        Some(path)
      else
        val outerAccessor = synthesizedPlacedValues(symbol, defn.AnyClass).symbol.fieldMembers find: member =>
          member.isParamAccessor && member.info.typeSymbol == synthesizedPlacedValues(symbol.owner, defn.AnyClass).symbol
        outerAccessor flatMap { outerAccessor => multitierOuterAccess(symbol.owner, path.select(outerAccessor)) }

    multitierOuterAccess(from, This(synthesizedPlacedValues(from, peer).symbol))
  end multitierOuterAccess

  def multitierAccessPath(path: Term, from: Symbol, peer: Symbol): Option[Term] = path match
    case Ident(_) if isMultitierModule(path.symbol) && from.hasAncestor(path.symbol.moduleClass) =>
      multitierOuterAccess(from, path.symbol.moduleClass, peer)
    case This(_) if isMultitierModule(path.symbol) && from.hasAncestor(path.symbol) =>
      multitierOuterAccess(from, path.symbol, peer)
    case Select(qualifier, _) =>
      if isMultitierModule(path.symbol) &&
         !isMultitierNestedPath(qualifier.symbol) &&
         isStablePath(qualifier) &&
         path.symbol.moduleClass.exists &&
         from.hasAncestor(path.symbol.moduleClass) then
        multitierOuterAccess(from, path.symbol.moduleClass, peer)
      else if isMultitierNestedPath(qualifier.symbol) then
        synthesizedDefinitions(path.symbol) flatMap: nestedModule =>
          multitierAccessPath(qualifier, from, peer) map { _.select(nestedModule.binding) }
      else
        None
    case _ =>
      None
  end multitierAccessPath
end AccessPath
