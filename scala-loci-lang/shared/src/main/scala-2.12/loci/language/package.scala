package loci

import scala.annotation.showAsInfix

package object language {
  @showAsInfix type from[T, R] = PlacedValue[T, R]
  @showAsInfix type fromSingle[T, P] = Placed.Selection.Single[T, P]
  @showAsInfix type fromMultiple[T, P] = Placed.Selection.Multiple[T, P]

  def erased: Nothing = erased()
  def erased(ev: Any*): Nothing =
    throw new NotImplementedError("Erased language constructs should never be used")
}
