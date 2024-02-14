package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental

@experimental
trait Access:
  this: Component & Commons & Synthesis =>
  import quotes.reflect.*

  private def multitierOuterAccess(from: Symbol, to: Symbol) =
    def multitierOuterAccess(symbol: Symbol, path: Term): Option[Term] =
      if symbol == to then
        Some(path)
      else
        val outerAccessor = placedValuesSymbol(symbol, defn.AnyClass).declaredFields find { _.isParamAccessor }
        outerAccessor flatMap { outerAccessor => multitierOuterAccess(symbol.owner, path.select(outerAccessor)) }

    multitierOuterAccess(from, This(placedValuesSymbol(from, defn.AnyClass)))
  end multitierOuterAccess

  def multitierAccessPath(path: Term, from: Symbol): Option[Term] = path match
    case This(_) =>
      if isMultitierModule(path.symbol) && from.hasAncestor(path.symbol) then
        multitierOuterAccess(from, path.symbol)
      else
        None
    case Select(qualifier, _) =>
      if isMultitierModule(path.symbol) &&
         !isMultitierNestedPath(qualifier.symbol) &&
         isStablePath(qualifier) &&
         path.symbol.moduleClass.exists &&
         from.hasAncestor(path.symbol.moduleClass) then
        multitierOuterAccess(from, path.symbol.moduleClass)
      else if isMultitierNestedPath(qualifier.symbol) then
        multitierAccessPath(qualifier, from) map { _.select(synthesizedDefinitions(path.symbol).binding) }
      else
        None
    case _ =>
      None
  end multitierAccessPath
end Access
