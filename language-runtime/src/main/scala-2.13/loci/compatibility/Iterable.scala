package loci
package compatibility

object iterable {
  @inline private[loci] def collectFirst[T, U](iterable: IterableOnce[T])(pf: PartialFunction[T, U]) =
    iterable.iterator collectFirst pf

  @inline private[loci] def exists[T](iterable: IterableOnce[T])(p: T => Boolean) =
    iterable.iterator exists p
}
