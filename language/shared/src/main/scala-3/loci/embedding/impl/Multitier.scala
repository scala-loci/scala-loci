package loci
package embedding
package impl

import components.*

import scala.quoted.*

object Multitier:
  def annotation(using annotationQuotes: Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect.*

    object processor extends Component
        with Commons
        with ErrorReporter
        with Annotations
        with PlacementInfo
        with PlacementContextTypes
        with PlacedExpressions
        with Splitting:
      val quotes: annotationQuotes.type = annotationQuotes

    tree match
      case tree: ClassDef =>
        val phases = List(
          processor.normalizePlacementContextTypes,
          processor.erasePlacementTypesFromExpressions,
          processor.split)

        val processed = phases.foldLeft(tree): (tree, process) =>
          if processor.canceled then tree else process(tree)

        processor.reportErrors()

        println(processed.show)

        List(processed)

      case _ =>
        report.errorAndAbort("multitier annotation only applicable to classes, traits or objects")
  end annotation
end Multitier
