package loci
package embedding

import scala.annotation.{compileTimeOnly, implicitNotFound}
import scala.annotation.unchecked.uncheckedVariance

infix type of[T <: Nothing, P] = T { type on = P }
infix type on[T, P] = Placed[P, T] & T
infix type from[T, R] = PlacedValue.Resolution[R, T]
infix type fromSingle[T, P] = Placed.Selection.Single[P, T]
infix type fromMultiple[T, P] = Placed.Selection.Multiple[P, T]

object Placement:
  @implicitNotFound("Expression must be placed on a peer")
  sealed trait Context[+P]:
    private[Context] type Peer = P @uncheckedVariance

  object Context:
    type Resolution[P] = Context[P] { type Peer = P }

    @compileTimeOnly("Illegal use of multitier construct")
    def fallback[P]: Context[P] = erased

    @compileTimeOnly("Illegal use of multitier construct")
    given fallback[P](using Context[?]): Context[P] = erased

    sealed trait ResolutionWithFallback[P]

    sealed trait ResolutionWithFallbackFallback:
      @compileTimeOnly("Illegal use of multitier construct")
      given fallback[P]: ResolutionWithFallback[P] = erased

    object ResolutionWithFallback extends ResolutionWithFallbackFallback:
      @compileTimeOnly("Illegal use of multitier construct")
      given resolution[P](using Resolution[P]): ResolutionWithFallback[P] = erased
    end ResolutionWithFallback
  end Context
end Placement
