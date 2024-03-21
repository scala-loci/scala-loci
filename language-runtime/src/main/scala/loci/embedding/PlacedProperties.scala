package loci
package embedding

import loci.language._

import scala.annotation.implicitNotFound


sealed trait Subjectivity[-T, U]

sealed trait SubjectivityDefault {
  implicit def default[T]: Subjectivity[Local[T], T] = erased
}

sealed trait SubjectivitySubjective extends SubjectivityDefault {
  implicit def subjective[T, P]: Subjectivity[T per P, T] = erased
}

object Subjectivity extends SubjectivitySubjective {
  implicit def nothingSubjective[P]: Subjectivity[Nothing per P, Nothing] = erased
  implicit def nothing: Subjectivity[Nothing, Nothing] = erased
}


@implicitNotFound("Could not resolve multiplicity for remote value through selection type or tie")
sealed trait Multiplicity[L, R, V, T, M]

sealed trait MultiplicityTie {
  implicit def tie[L, R, T, M, N](implicit ev0: Tie[L, R, N], ev1: M =:= N): Multiplicity[L, R, T, T, M] = erased(ev0, ev1)
}

sealed trait MultiplicitySelection extends MultiplicityTie {
  implicit def single[L, R, T, N](implicit ev: Tie[L, R, N]): Multiplicity[L, R, Placed.Selected.Single[T], T, Tie.Single] = erased(ev)
  implicit def multiple[L, R, T, N](implicit ev: Tie[L, R, N]): Multiplicity[L, R, Placed.Selected.Multiple[T], T, Tie.Multiple] = erased(ev)
}

object Multiplicity extends MultiplicitySelection {
  implicit def nothing[L, R, M, N](implicit ev0: Tie[L, R, N], ev1: M =:= N): Multiplicity[L, R, Nothing, Nothing, M] = erased(ev0, ev1)
}


@implicitNotFound("No tie specified from ${L} to ${R}")
sealed trait Tie[L, R, M]

sealed trait TieMultiple {
  implicit def multiple[L, R](implicit ev: L <:< Any { type Tie <: Multiple[R] }): Tie[L, R, Tie.Multiple] = erased(ev)
}

sealed trait TieOptional extends TieMultiple {
  implicit def optional[L, R](implicit ev: L <:< Any { type Tie <: Optional[R] }): Tie[L, R, Tie.Optional] = erased(ev)
}

sealed trait TieSingle extends TieOptional {
  implicit def single[L, R](implicit ev: L <:< Any { type Tie <: Single[R] }): Tie[L, R, Tie.Single] = erased(ev)
}

object Tie extends TieSingle {
  type Single
  type Optional
  type Multiple
}
