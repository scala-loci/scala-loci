package loci
package runtime

import transmitter.Marshallable

import scala.annotation.StaticAnnotation
import scala.annotation.meta.{getter, setter}

final class MarshallableValue[B, I, R, P](
  val marshallable: Marshallable[B, R, P])

@getter @setter
final class MarshallableInfo(signature: Int) extends StaticAnnotation

final class PlacedValue[U, R, B, T](
  val signature: Value.Signature,
  val stable: Boolean,
  val arguments: Marshallable[U, R, _],
  val result: Marshallable[B, _, T])

@getter @setter
final class PlacedValueInfo(
  signature: String,
  arguments: MarshallableValue[_, _, _, _],
  result: MarshallableValue[_, _, _, _]) extends StaticAnnotation
