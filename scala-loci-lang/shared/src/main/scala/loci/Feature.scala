package loci

object feature {
  implicit object manualImplicitContext extends language.feature.ManualImplicitContext
  implicit object manualSelectionTupling extends language.feature.ManualSelectionTupling
}
