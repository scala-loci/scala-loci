package loci

package object compatibility {
  private[loci] type IterableOnce[T] = scala.collection.TraversableOnce[T]
  private[loci] val IterableOnce = scala.collection.TraversableOnce

  private[loci] type Iterable[T] = scala.collection.Traversable[T]
  private[loci] val Iterable = scala.collection.Traversable
}

package compatibility {
  private[loci] class nowarn(value: String = "") extends scala.annotation.StaticAnnotation
}
