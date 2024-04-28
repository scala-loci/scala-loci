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
      RemoteAccessorGeneration,
      Invocation

    import processor.*

    val preprocessingPhases = List(
      normalizePlacedStatements,
      liftRemoteBlocks,
      eraseMultitierConstructs)

    val processingPhases = List(
      enterSynthesizedSymbols,
      split,
      rewireInvocations,
      addAccessors)

    class Preprocessor extends SafeTreeMap(quotes):
      def trySwapMultitierAnnotation(symbol: Symbol) =
        SymbolMutator.get foreach: symbolMutator =>
          val instantiation = New(TypeIdent(symbols.`embedding.multitier`))
          val annotation =
            multitierModuleArgument(symbol) match
              case Some(arg) => instantiation.select(symbols.`embedding.multitier`.declarations.last).appliedTo(arg)
              case _ => instantiation.select(symbols.`embedding.multitier`.declarations.head).appliedToNone
          symbolMutator.removeAnnotation(symbol, symbols.`language.multitier`)
          symbolMutator.updateAnnotationWithTree(symbol, annotation)

      override def transformTerm(term: Term)(owner: Symbol) =
        if canceled then term else super.transformTerm(term)(owner)

      override def transformStatement(stat: Statement)(owner: Symbol) = stat match
        case stat: ValDef if stat.symbol.hasAnnotation(symbols.`language.multitier`) =>
          trySwapMultitierAnnotation(stat.symbol)
          super.transformStatement(stat)(owner)

        case stat: ClassDef if isMultitierModule(stat.symbol) =>
          trySwapMultitierAnnotation(stat.symbol)

          val preprocessed = preprocessingPhases.foldLeft(stat): (stat, process) =>
            if canceled then stat else process(stat)

          if canceled then preprocessed else super.transformStatement(preprocessed)(owner)

        case stat =>
          if canceled then stat else super.transformStatement(stat)(owner)
    end Preprocessor

    class Processor(skip: Boolean) extends SafeTreeMap(quotes):
      override def transformStatement(stat: Statement)(owner: Symbol) = stat match
        case stat: ClassDef if isMultitierModule(stat.symbol) =>
          val processed =
            if skip then stat
            else processingPhases.foldLeft(stat) { (stat, process) => process(stat) }

          APIExtraction.extractAPI(processed)
          super.transformStatement(processed)(owner)

        case stat =>
          super.transformStatement(stat)(owner)
    end Processor

    tree match
      case _: ClassDef =>
        if !(tree.symbol.owner hasAncestor isMultitierModule) then
          object preprocessor extends Preprocessor
          val preprocessed @ ClassDef(_, _, _, _, _) = preprocessor.transformStatement(tree)(tree.symbol.owner): @unchecked

          object processor extends Processor(canceled)
          val processed @ ClassDef(_, _, _, _, _) = processor.transformStatement(preprocessed)(tree.symbol.owner): @unchecked

          reportErrors()
          List(processed)
        else
          List(tree)

      case _: ValDef if tree.symbol.owner hasAncestor isMultitierModule =>
        List(tree)

      case _ =>
        report.error("@multitier annotation is only applicable to classes, traits or objects.")
        List(tree)
  end annotation
end Multitier
