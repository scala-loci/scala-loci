package loci.dev

import loci.dev.language._

package object transmitter {
  type from[+T, -R] = PlacedValue[T, R]

  type Single = Tie.Single
  type Optional = Tie.Optional
  type Multiple = Tie.Multiple
}
