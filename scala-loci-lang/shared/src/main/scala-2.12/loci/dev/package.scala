package loci

import loci.dev.language._

import scala.annotation.{StaticAnnotation, showAsInfix}


package dev {
  final class peer extends StaticAnnotation
  final class multitier extends StaticAnnotation

  sealed trait Single[P] extends Multiple[P]
  sealed trait Optional[P] extends Multiple[P]
  sealed trait Multiple[+P]

  trait Remote[+P]
}

package object dev {
  type Local[T] = T

  @showAsInfix type on[T, P] = Placed[T, P] with T
  @showAsInfix type per[T, P] = Placed.Subjective[T, P]

  def placed: Placement.Placed = erased
  def on: Placement.Select[Placement.Run] = erased
  def on[P]: Placement.On[P] with Placement.Run[P, language.from] = erased
  def remote: Placement.Narrow with Placement.Select[Placement.Call] with Placement.Call[Nothing, language.from] = erased
  def remote[P]: Placement.Call[P, language.from] = erased
}
