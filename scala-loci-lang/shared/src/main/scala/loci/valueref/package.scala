package loci

import scala.annotation.showAsInfix

package object valueref extends ValueRefCreators with ValueRefAccessors with CompileTimeDummyImplicits {
  @showAsInfix type via[T, P] = ValueRef[T, P]
}
