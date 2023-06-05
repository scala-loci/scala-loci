package loci

import embedding._

import scala.annotation.{StaticAnnotation, showAsInfix}

package language {
  final class peer extends StaticAnnotation

  sealed trait Single[P] extends Multiple[P]
  sealed trait Optional[P] extends Multiple[P]
  sealed trait Multiple[+P]

  trait Remote[+P] extends Equals
  object Remote extends transmitter.RemoteReference
}

package object language {
  type Local[T] = T

  @showAsInfix type on[T, P] = Placed[T, P] with T
  @showAsInfix type per[T, P] = Placed.Subjective[T, P]
}
