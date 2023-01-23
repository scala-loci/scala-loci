package loci
package compatibility

import scala.collection.mutable

object listBuffer:
  private[loci] inline def mapInPlace[T](listBuffer: mutable.ListBuffer[T])(f: T => T) =
    listBuffer mapInPlace f
