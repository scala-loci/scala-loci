package loci
package runtime

import transmitter.Marshallable

import scala.annotation.StaticAnnotation
import scala.annotation.meta.{getter, setter}

@getter @setter
final class MarshallableInfo[I](signature: Int) extends StaticAnnotation

final class PlacedValue[U, R, B, T](
  val signature: Value.Signature,
  val stable: Boolean,
  val arguments: Marshallable[U, R, _],
  val result: Marshallable[B, _, T])

@getter @setter
final class PlacedValueInfo(
  signature: String,
  arguments: Marshallable[_, _, _],
  result: Marshallable[_, _, _]) extends StaticAnnotation
