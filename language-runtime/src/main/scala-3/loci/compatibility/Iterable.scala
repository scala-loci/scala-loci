package loci
package compatibility

object iterable:
  private[loci] inline def collectFirst[T, U](iterable: IterableOnce[T])(pf: PartialFunction[T, U]) =
    iterable.iterator collectFirst pf

  private[loci] inline def exists[T](iterable: IterableOnce[T])(p: T => Boolean) =
    iterable.iterator exists p
