package loci

import scala.annotation.showAsInfix

package object valueref extends ValueRefCreators {
  @showAsInfix type via[T, P] = ValueRef[T, P]
}
