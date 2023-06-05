package loci
package embedding

import scala.annotation.compileTimeOnly

infix type on[T, P] = Placed[P, T] & T
infix type from[T, R] = PlacedValue[R, T]
infix type fromSingle[T, P] = Placed.Selection.Single[P, T]
infix type fromMultiple[T, P] = Placed.Selection.Multiple[P, T]

object Placement:
  sealed trait Context[P]

  sealed trait ContextFallback:
    @compileTimeOnly("Expression must be placed on a peer")
    given fallback[P]: Context[P]()

  object Context extends ContextFallback:
    @compileTimeOnly("Expression must be placed on a peer")
    given Context[Any]()
end Placement
