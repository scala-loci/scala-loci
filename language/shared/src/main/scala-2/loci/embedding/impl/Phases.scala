package loci
package embedding
package impl

import scala.collection.mutable

object Phases {
  sealed trait SortResult
  case class Sorted(phases: List[Phase]) extends SortResult
  case class CyclicDependency(cycle: List[String]) extends SortResult
  case class DuplicateIdentifier(name: String) extends SortResult
  case class InvalidIdentifier(name: String) extends SortResult
  case class InvalidReference(name: String) extends SortResult

  def sort(phases: Seq[Phase]): SortResult =
    validate(phases) getOrElse sort(order(phases), edges(phases))

  private def validate(phases: Seq[Phase]) = {
    def invalidName(name: String) =
      name.trim.isEmpty || (name exists { ch =>
        ("-+*/\\:" indexOf ch) == -1 && !Character.isJavaIdentifierPart(ch)
      })

    val names = phases.foldLeft[Either[String, Set[String]]](Right(Set.empty)) {
      case (left @ Left(_), _) =>
        left
      case (Right(names), phase) =>
        val name = phase.name
        if (names contains name) Left(name) else Right(names + name)
    }

    names match {
      case Left(name) => Some(DuplicateIdentifier(name))
      case Right(names) =>
        (names
          collectFirst { case name if invalidName(name) => InvalidIdentifier(name) }
          orElse (phases
            flatMap { phase =>
              phase.before ++ phase.after collect {
                case name if name != "*" =>
                  if (name startsWith "?") Left(name.substring(1)) else Right(name)
              }
            }
            collectFirst {
              case Left(name) if invalidName(name) => InvalidIdentifier(name)
              case Right(name) if invalidName(name) => InvalidIdentifier(name)
              case Right(name) if !(names contains name) => InvalidReference(name)
            }))
    }
  }

  private def order(phases: Seq[Phase]) = {
    val partitioned = phases map { phase =>
      val phaseBeforeWildcard = phase.before contains "*"
      val phaseAfterWildcard = phase.after contains "*"

      if (phaseBeforeWildcard && phase.after.isEmpty)
        0 -> phase
      else if (phaseBeforeWildcard && !phaseAfterWildcard)
        1 -> phase
      else if (phaseAfterWildcard && phase.before.isEmpty)
        4 -> phase
      else if (phaseAfterWildcard && !phaseBeforeWildcard)
        3 -> phase
      else
        2 -> phase
    }

    (partitioned collect { case (4, phase) => phase }) ++
    (partitioned collect { case (3, phase) => phase }) ++
    (partitioned collect { case (2, phase) => phase }).reverse ++
    (partitioned collect { case (1, phase) => phase }).reverse ++
    (partitioned collect { case (0, phase) => phase }).reverse
  }

  private def edges(phases: Seq[Phase]) = (phases
    combinations 2
    flatMap { phases =>
      val Seq(p0, p1) = phases: @unchecked
      Seq(p0 -> p1, p1 -> p0)
    }
    filter { case (p0, p1) =>
      (p0.before contains p1.name) || (p1.after contains p0.name) ||
      (p0.before contains "?" + p1.name) || (p1.after contains "?" + p0.name)
    }).toList

  private def sort(phases: Seq[Phase], edges: Seq[(Phase, Phase)]) = {
    val permanents = mutable.Set.empty[Phase]
    val temporaries = mutable.Set.empty[Phase]
    var list = List.empty[Phase]
    var error = List.empty[String]

    def visit(phase: Phase, path: List[String]): Boolean =
      if (permanents contains phase)
        true
      else if (temporaries contains phase) {
        error = phase.name :: path
        false
      }
      else {
        temporaries += phase
        val result = edges forall {
          case (`phase`, succeeding) => visit(succeeding, phase.name :: path)
          case _ => true
        }
        permanents += phase
        list ::= phase
        result
      }

    if (phases forall { visit(_, List.empty) })
      Sorted(list)
    else
      CyclicDependency(error)
  }
}
