package loci
package embedding
package impl
package components

import scala.annotation.experimental

@experimental
trait Annotations:
  this: Component & Commons =>
  import quotes.reflect.*

  def contextResultCount(symbol: Symbol) =
    symbol.getAnnotation(symbols.contextResultCount) match
      case Some(Apply(_, List(Literal(IntConstant(count))))) => count
      case _ => 0

//  def canIncrementContextResultCount = SymbolMutator.get.isDefined
//
//  def tryIncrementContextResultCount(symbol: Symbol) =
//    SymbolMutator.get foreach: symbolMutator =>
//      symbolMutator.updateAnnotation(
//        symbol,
//        symbols.contextResultCount,
//        List(Literal(IntConstant(contextResultCount(symbol) + 1))))

//  def canDecrementContextResultCount = SymbolMutator.get.isDefined
//
//  def tryDecrementContextResultCount(symbol: Symbol) =
//    val count = contextResultCount(symbol) - 1
//    if count >= 0 then
//      SymbolMutator.get foreach: symbolMutator =>
//        if count > 0 then
//          symbolMutator.updateAnnotation(
//            symbol,
//            symbols.contextResultCount,
//            List(Literal(IntConstant(count))))
//        else
//          symbolMutator.removeAnnotation(symbol, symbols.contextResultCount)

//  def canSetContextResultCount = SymbolMutator.get.isDefined
//
//  def trySetContextResultCount(symbol: Symbol, count: Int) =
//    SymbolMutator.get foreach: symbolMutator =>
//      symbolMutator.updateAnnotation(
//        symbol,
//        symbols.contextResultCount,
//        List(Literal(IntConstant(count))))

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
