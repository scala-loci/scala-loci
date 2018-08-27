package loci.dev
package language
package impl

class Phase(
    val name: String,
    val transform: List[Any] => List[Any],
    val before: Set[String] = Set.empty,
    val after: Set[String] = Set.empty)

object Phase {
  def apply(name: String,
    transform: List[Any] => List[Any],
    before: Set[String] = Set.empty,
    after: Set[String] = Set.empty) = new Phase(name, transform, before, after)
}
