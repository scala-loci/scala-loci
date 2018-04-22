package loci
package impl

sealed trait TieMultiplicity
case object SingleTie extends TieMultiplicity
case object OptionalTie extends TieMultiplicity
case object MultipleTie extends TieMultiplicity
