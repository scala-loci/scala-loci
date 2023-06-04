package loci
package embedding

import scala.annotation.compileTimeOnly

object Placement:
  sealed trait Context[P]

  sealed trait ContextFallback:
    @compileTimeOnly("Expression must be placed on a peer")
    given fallback[P]: Context[P]()

  object Context extends ContextFallback:
    @compileTimeOnly("Expression must be placed on a peer")
    given Context[Any]()
end Placement
