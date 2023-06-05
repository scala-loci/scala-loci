package loci
package language

import embedding.*

import scala.annotation.StaticAnnotation


final class peer extends StaticAnnotation

sealed trait Single[P] extends Multiple[P]
sealed trait Optional[P] extends Multiple[P]
sealed trait Multiple[+P]

trait Remote[+P] extends Equals
object Remote extends transmitter.RemoteReference


type Local[T] = T

infix type on[T, P] = Placement.Context[P] ?=> embedding.on[T, P]
infix type per[T, P] = Placed.Subjective[P, T]
