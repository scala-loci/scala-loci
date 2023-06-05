package loci
package embedding

import scala.annotation.implicitNotFound

object Placement {
  @implicitNotFound("Expression must be placed on a peer")
  sealed trait Context[P]
}
