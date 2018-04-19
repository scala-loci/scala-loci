package loci
package util

import scala.collection.mutable.LinkedHashMap

final class Attributes(attributes: TraversableOnce[(String, String)]) {
  private val attrs = LinkedHashMap.empty[String, Value]

  attributes foreach { case (key, value) =>
    val values = (attrs getOrElse (key, Value.empty)).values
    attrs += key -> Value(values :+ value)
  }

  def apply(key: String): Option[Value] = attrs get key

  def toSeq: Seq[(String, String)] = attrs.toSeq flatMap { case (key, value) =>
    value.values map { (key, _) }
  }

  override def toString = (toSeq
    map { case (key, value) => s"$key -> $value" }
    mkString ("Attributes(", ", ", ")"))
}

final class Value(val values: List[String]) extends AnyVal

object Attributes {
  def empty = new Attributes(Seq.empty)
  def apply(attributes: TraversableOnce[(String, String)]): Attributes =
    new Attributes(attributes)
  def apply(attributes: (String, String)*): Attributes =
    new Attributes(attributes)
}

object Value {
  def empty: Value = new Value(List.empty)
  def apply(value: String): Value =
    new Value(List(value))
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
