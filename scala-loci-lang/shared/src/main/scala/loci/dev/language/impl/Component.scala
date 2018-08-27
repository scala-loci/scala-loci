package loci.dev
package language
package impl

import scala.language.higherKinds
import scala.reflect.macros.blackbox

trait Component[C <: blackbox.Context] {
  val engine: Engine[C]
  val phases: Seq[Phase]
}

object Component {
  type AnyFactory = Factory[Component]

  abstract class Factory[+Comp[C <: blackbox.Context] <: Component[C]](
    val requires: Seq[AnyFactory] = Seq.empty) {

    def asInstance[C <: blackbox.Context]: PartialFunction[Component[C], Comp[C]]
    def apply[C <: blackbox.Context](engine: Engine[C]): Comp[C]
  }
}
