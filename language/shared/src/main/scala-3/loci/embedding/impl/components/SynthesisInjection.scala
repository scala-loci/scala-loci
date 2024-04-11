package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.collection.mutable

@experimental
trait SynthesisInjection:
  this: Component & Commons & Peers & PlacedValueSynthesis =>
  import quotes.reflect.*

  def enterSynthesizedSymbols(module: ClassDef): ClassDef =
    object multitierModuleRefsCollector extends TreeAccumulator[Set[Symbol]]:
      def foldTree(modules: Set[Symbol], tree: Tree)(owner: Symbol) =
        if isMultitierModule(tree.symbol) || tree.symbol.isModuleDef && (tree.symbol hasAncestor isMultitierModule) then
          foldOverTree(modules + tree.symbol, tree)(owner)
        else
          foldOverTree(modules, tree)(owner)

    val modules = multitierModuleRefsCollector.foldTree(Set.empty, module)(module.symbol.owner)
    val sortedModules = (modules flatMap { _.typeRef.baseClasses }).toList sortWith { _.typeRef derivesFrom _ }

    val modifiedModules = mutable.Set.empty[Symbol]

    sortedModules.reverseIterator foreach: module =>
      val peers =
        if isMultitierModule(module) then
          if module.typeRef.baseClasses exists { modifiedModules contains _ } then
            SymbolMutator.getOrErrorAndAbort.invalidateMemberCaches(module)

          PeerInfo.ofModule(module) map { _.peerType.typeSymbol }
        else if module.isModuleDef && (module hasAncestor isMultitierModule) then
          List(defn.AnyClass)
        else
          List.empty
      end peers

      peers foreach: peer =>
        val symbol = synthesizedPlacedValues(module, peer).symbol
        if !(module.declarations contains symbol) then
          SymbolMutator.getOrErrorAndAbort.enter(module, symbol)
          modifiedModules += module

    module
  end enterSynthesizedSymbols
end SynthesisInjection
