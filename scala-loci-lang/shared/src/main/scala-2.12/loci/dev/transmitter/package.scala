package loci.dev

import loci.dev.language._

import scala.annotation.showAsInfix

package object transmitter {
  @showAsInfix type from[+T, -R] = PlacedValue[T, R]

  type Single = Tie.Single
  type Optional = Tie.Optional
  type Multiple = Tie.Multiple
}
