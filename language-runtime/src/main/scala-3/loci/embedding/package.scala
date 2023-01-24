package loci
package embedding

import loci.language.*

infix type on[T, P] = Placed.Value[T] match
  case Placed.Value[t per r] => Placed.Subjective.on[t per r, P]
  case _ => Placed.on[T, P]
infix type from[T, R] = PlacedValue[R, T]
infix type fromSingle[T, P] = Placed.Selection.Single[P, T]
infix type fromMultiple[T, P] = Placed.Selection.Multiple[P, T]

def erased: Nothing = erased()
def erased(ev: Any*): Nothing =
  throw new NotImplementedError("Erased language constructs should never be used")
