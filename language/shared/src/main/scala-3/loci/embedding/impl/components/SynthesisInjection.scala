package loci
package embedding
package impl
package components

import scala.annotation.experimental

@experimental
trait SynthesisInjection:
  this: Component & Commons & PlacedValueSynthesis =>
  import quotes.reflect.*

  def enterSynthesizedSymbols(module: ClassDef): ClassDef =
    module
end SynthesisInjection
