package retier
package util

final class Attributes(attributes: TraversableOnce[(String, String)]) {
  private val attrs = attributes.foldLeft(Map.empty[String, Value]) {
    case (map, (key, value)) =>
      map + (key -> Value((map getOrElse (key, Value.empty)).values :+ value))
  }

  def apply(key: String): Option[Value] = attrs get key
}

final class Value(val values: List[String]) extends AnyVal

object Attributes {
  def empty = new Attributes(Seq.empty)
  def apply(attributes: TraversableOnce[(String, String)]): Attributes =
    new Attributes(attributes)
}

object Value {
  def empty: Value = new Value(List.empty)
  def apply(values: TraversableOnce[String]): Value =
    new Value(values.toList)
  def unapply(attribute: Value): Option[String] =
    attribute.values.headOption
  def unapply(attribute: Option[Value]): Option[String] =
    attribute flatMap { unapply(_) }
}

object Values {
  def unapply(attribute: Value): Option[List[String]] =
    Some(attribute.values)
  def unapply(attribute: Option[Value]): Option[List[String]] =
    attribute flatMap { unapply(_) }
}
