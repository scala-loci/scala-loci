package loci.dev
package runtime

import loci.transmitter.Marshallable

import scala.annotation.StaticAnnotation

final class MarshallableInfo[I](signature: Int) extends StaticAnnotation

final class PlacedValue[AB, AR, AP, RB, RR, RP](
  val name: String,
  val stable: Boolean,
  val arguments: Marshallable[AB, AR, AP],
  val result: Marshallable[RB, RR, RP])

final class PlacedValueInfo(
  signature: String,
  arguments: Marshallable[_, _, _],
  result: Marshallable[_, _, _]) extends StaticAnnotation
