package loci
package compatibility

private[loci] type IterableOnce[T] = scala.collection.IterableOnce[T]
private[loci] val IterableOnce = scala.collection.IterableOnce

private[loci] type Iterable[T] = scala.collection.Iterable[T]
private[loci] val Iterable = scala.collection.Iterable

private[loci] type nowarn = scala.annotation.nowarn
