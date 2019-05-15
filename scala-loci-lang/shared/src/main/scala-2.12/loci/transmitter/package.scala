package loci

import scala.annotation.showAsInfix

package object transmitter {
  @showAsInfix type from[+T, -R] = language.PlacedValue[T, R]

  type Gateway[R] = language.Gateway[R]

  type Single = language.Tie.Single
  type Optional = language.Tie.Optional
  type Multiple = language.Tie.Multiple
}
