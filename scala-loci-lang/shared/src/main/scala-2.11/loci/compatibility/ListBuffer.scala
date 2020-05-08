package loci
package compatibility

import scala.collection.mutable

object listBuffer {
  @inline private[loci] def mapInPlace[T](listBuffer: mutable.ListBuffer[T])(f: T => T) =
    listBuffer transform f
}
