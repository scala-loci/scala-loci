package retier
package impl
package engine

import scala.reflect.runtime.universe._

final class Aggregator[+T] private (private val list: List[(Type, Any)]) {
  import Aggregator._

  def add[U: TypeTag](elems: TraversableOnce[U]): Aggregator[T with U] =
    new Aggregator(list ++ (elems map { (tpeOf[U], _) }))

  def replace[U: TypeTag](elems: TraversableOnce[U]): Aggregator[T with U] =
    new Aggregator(
      (list filterNot { _._1 =:= tpeOf[U] }) ++ (elems map { (tpeOf[U], _) }))

  def all[U: TypeTag](implicit ev: T <:< U): List[U] =
    list filter { _._1 =:= tpeOf[U] } map { _._2.asInstanceOf[U] }

  def aggregate[In >: T, Out <: In]
      (f: Aggregator[In] => Aggregator[Out]): Aggregator[T with Out] =
    f(this).asInstanceOf[Aggregator[T with Out]]
}

object Aggregator {
  def empty: Aggregator[Any] =
    new Aggregator(List.empty)

  def create[U: TypeTag](elems: TraversableOnce[U]): Aggregator[Any with U] =
    new Aggregator((elems map { (tpeOf[U], _) }).toList)

  private def tpeOf[U: TypeTag] = {
    // translate to a more general type (without self references)
    // if the type takes no type arguments.
    // It does not seem easily possible for types with arguments.
    val tpe = typeOf[U]
    val symbol = tpe.typeSymbol
    val symbolTpe = symbol.asType.toType
    if (!symbolTpe.takesTypeArgs) symbolTpe else tpe
  }
}
