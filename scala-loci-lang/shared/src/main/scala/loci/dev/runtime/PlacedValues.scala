package loci.dev
package runtime

import scala.annotation.compileTimeOnly

trait PlacedValues extends Any

object PlacedValues {
  @compileTimeOnly("Multitier type can only be instantiated inside placed expression")
  implicit def placedValues[T <: PlacedValues]: T =
    throw new NotImplementedError("Multitier type can only be instantiated inside placed expression")
}
