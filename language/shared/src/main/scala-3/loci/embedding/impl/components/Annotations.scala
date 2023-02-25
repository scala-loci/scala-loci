package loci
package embedding
package impl
package components

import scala.quoted.*

trait Annotations:
  this: Component with Commons =>
  import quotes.reflect.*

  def contextResultCount(symbol: Symbol) =
    symbol.getAnnotation(symbols.contextResultCount) match
      case Some(Apply(_, List(Literal(IntConstant(count))))) => count
      case _ => 0

  def tryIncrementContextResultCount(symbol: Symbol) =
    SymbolMutator.get foreach: symbolMutator =>
      symbolMutator.updateAnnotation(
        symbol,
        symbols.contextResultCount,
        List(Literal(IntConstant(contextResultCount(symbol) + 1))))

  def trySetContextResultCount(symbol: Symbol, count: Int) =
    SymbolMutator.get foreach: symbolMutator =>
      symbolMutator.updateAnnotation(
        symbol,
        symbols.contextResultCount,
        List(Literal(IntConstant(count))))
end Annotations
