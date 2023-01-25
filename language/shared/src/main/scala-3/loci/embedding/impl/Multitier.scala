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
        with Annotations
        with PlacementInfo
        with PlacementContextTypesNormalization:
      val quotes: annotationQuotes.type = annotationQuotes

    tree match
      case tree: ClassDef =>
        List(processor.normalizePlacementContextTypes(tree))
      case _ =>
        report.errorAndAbort("multitier annotation only applicable to classes, traits or objects")
