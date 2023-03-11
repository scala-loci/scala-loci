package loci
package embedding
package impl
package components

trait Annotations:
  this: Component with Commons =>
  import quotes.reflect.*

  def contextResultCount(symbol: Symbol) =
    symbol.getAnnotation(symbols.contextResultCount) match
      case Some(Apply(_, List(Literal(IntConstant(count))))) => count
      case _ => 0

  def canIncrementContextResultCount = SymbolMutator.get.isDefined

  def tryIncrementContextResultCount(symbol: Symbol) =
    SymbolMutator.get foreach: symbolMutator =>
      symbolMutator.updateAnnotation(
        symbol,
        symbols.contextResultCount,
        List(Literal(IntConstant(contextResultCount(symbol) + 1))))

  def canSetContextResultCount = SymbolMutator.get.isDefined

  def trySetContextResultCount(symbol: Symbol, count: Int) =
    SymbolMutator.get foreach: symbolMutator =>
      symbolMutator.updateAnnotation(
        symbol,
        symbols.contextResultCount,
        List(Literal(IntConstant(count))))

  def canMakeCompileTimeOnly = SymbolMutator.get.isDefined

  def tryMakeCompileTimeOnly(symbol: Symbol, message: String) =
    SymbolMutator.get foreach: symbolMutator =>
      symbolMutator.updateAnnotation(
        symbol,
        symbols.compileTimeOnly,
        List(Literal(StringConstant(message))))

  def canMakeTargetName = SymbolMutator.get.isDefined

  def tryMakeTargetName(symbol: Symbol, name: String) =
    SymbolMutator.get foreach: symbolMutator =>
      symbolMutator.updateAnnotation(
        symbol,
        symbols.targetName,
        List(Literal(StringConstant(name))))
end Annotations
