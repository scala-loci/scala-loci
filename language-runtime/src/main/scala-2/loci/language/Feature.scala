package loci
package language

object feature {
  implicit object manualImplicitContext extends embedding.feature.ManualImplicitContext
  implicit object manualSelectionTupling extends embedding.feature.ManualSelectionTupling
}
