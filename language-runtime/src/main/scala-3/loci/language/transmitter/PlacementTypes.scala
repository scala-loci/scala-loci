package loci
package language

package object transmitter {
  infix type from[+T, -R] = embedding.PlacedValue[R, T]

  type Gateway[R] = embedding.Gateway[R]

  type Single = embedding.Tie.Single
  type Optional = embedding.Tie.Optional
  type Multiple = embedding.Tie.Multiple
}
