package loci.dev
package language
package impl
package components

import scala.reflect.macros.blackbox

object ModuleInfo extends Component.Factory[ModuleInfo] {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new ModuleInfo(engine)
  def asInstance[C <: blackbox.Context] = { case c: ModuleInfo[C] => c }
}

class ModuleInfo[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq.empty

  object module {
    val tree = engine.multitierCode
    val symbol = tree.symbol
    val stats = tree.impl.body
  }
}
