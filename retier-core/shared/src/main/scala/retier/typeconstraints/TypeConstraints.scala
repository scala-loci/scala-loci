package retier
package typeconstraints

import scala.annotation.implicitNotFound

@implicitNotFound("Cannot prove that ${A} =:!= ${B}.")
final abstract class =:!=[A, B]

protected object =:!= {
  implicit def unequal[A, B](implicit ev: Unequal[A, B]): A =:!= B = null

  final abstract class Unequal[A, B]
  implicit def unequalEvidence[A, B]: Unequal[A, B] = null
  implicit def equalAmbiguousEvidence0[A]: Unequal[A, A] = null
  implicit def equalAmbiguousEvidence1[A]: Unequal[A, A] = null
}

@implicitNotFound("Cannot prove that ${A} <:!< ${B}.")
final abstract class <:!<[A, B]

protected object <:!< {
  implicit def nonSubtype[A, B](implicit ev: NonSubtype[A, B]): A <:!< B = null

  final abstract class NonSubtype[A, B]
  implicit def nonSubtypeEvidence[A, B]: NonSubtype[A, B] = null
  implicit def subtypeAmbiguousEvidence0[A, B >: A]: NonSubtype[A, B] = null
  implicit def subtypeAmbiguousEvidence1[A, B >: A]: NonSubtype[A, B] = null
}
