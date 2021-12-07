package loci
package language

package object transmitter {
  type from[+T, -R] = embedding.PlacedValue[T, R]

  type Gateway[R] = embedding.Gateway[R]

  type Single = embedding.Tie.Single
  type Optional = embedding.Tie.Optional
  type Multiple = embedding.Tie.Multiple
}
