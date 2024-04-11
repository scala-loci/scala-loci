package loci
package embedding
package impl

import components.*
import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.quoted.*

@experimental
object Multitier:
  def annotation(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect.*

    object processor extends
      Component.withQuotes(quotes),
      Commons,
      ErrorReporter,
      Annotations,
      Placements,
      Peers,
      AccessPath,
      PlacedTransformations,
      PlacedStatements,
      PlacedBlocks,
      PlacedExpressions,
      PlacedValueSynthesis,
      RemoteAccessorSynthesis,
      SynthesisInjection,
      PlacedValueSplitting,
      Invocation

    import processor.*

    val phases = List(
      normalizePlacedStatements,
      liftRemoteBlocks,
      eraseMultitierConstructs,
      enterSynthesizedSymbols,
      split,
      rewireInvocations)

    object expansion extends SafeTreeMap(quotes):
      override def transformStatement(stat: Statement)(owner: Symbol) = stat match
        case stat: ClassDef if isMultitierModule(stat.symbol) =>
          SymbolMutator.get foreach: symbolMutator =>
            val instantiation = New(TypeIdent(symbols.`embedding.multitier`))
            val annotation =
              multitierModuleArgument(stat.symbol) match
                case Some(arg) => instantiation.select(symbols.`embedding.multitier`.declarations.last).appliedTo(arg)
                case _ => instantiation.select(symbols.`embedding.multitier`.declarations.head).appliedToNone
            symbolMutator.removeAnnotation(stat.symbol, symbols.`language.multitier`)
            symbolMutator.updateAnnotationWithTree(stat.symbol, annotation)

          val processed = phases.foldLeft(stat): (stat, process) =>
            if canceled then stat else process(stat)

          APIExtraction.extractAPI(processed)
          super.transformStatement(processed)(owner)

        case stat =>
          super.transformStatement(stat)(owner)
    end expansion

    tree match
      case tree: ClassDef =>
        if !(tree.symbol.owner hasAncestor isMultitierModule) then
          val processed @ ClassDef(_, _, _, _, _) = expansion.transformStatement(tree)(tree.symbol.owner): @unchecked
          reportErrors()
          List(processed)
        else
          List(tree)

      case _ =>
        report.error("@multitier annotation is only applicable to classes, traits or objects.")
        List(tree)
  end annotation
end Multitier
