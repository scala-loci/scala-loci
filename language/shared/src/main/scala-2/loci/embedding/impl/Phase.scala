package loci
package embedding
package impl

class Phase(
    val name: String,
    val transform: List[Any] => List[Any],
    val after: Set[String] = Set.empty,
    val before: Set[String] = Set.empty)

object Phase {
  def apply(name: String,
    transform: List[Any] => List[Any],
    after: Set[String] = Set.empty,
    before: Set[String] = Set.empty) = new Phase(name, transform, after, before)
}
