package loci

import embedding._

import scala.annotation.StaticAnnotation

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

  type on[T, P] = Placed[T, P] with T
  type per[T, P] = Placed.Subjective[T, P]
}
