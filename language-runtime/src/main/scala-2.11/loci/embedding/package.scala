package loci

package object embedding {
  type from[T, R] = PlacedValue[T, R]
  type fromSingle[T, P] = Placed.Selection.Single[T, P]
  type fromMultiple[T, P] = Placed.Selection.Multiple[T, P]

  def erased: Nothing = erased()
  def erased(ev: Any*): Nothing =
    throw new NotImplementedError("Erased language constructs should never be used")
}
