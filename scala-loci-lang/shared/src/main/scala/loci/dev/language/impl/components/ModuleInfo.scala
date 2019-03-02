package loci.dev
package language
package impl
package components

import scala.reflect.macros.blackbox

object ModuleInfo extends Component.Factory[ModuleInfo](
    requires = Seq(Commons)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new ModuleInfo(engine)
  def asInstance[C <: blackbox.Context] = { case c: ModuleInfo[C] => c }
}

class ModuleInfo[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq.empty

  val commons = engine.require(Commons)

  import engine.c.universe._
  import commons._

  object module {
    val tree = engine.multitierCode
    val name = tree.name
    val className = name.toTypeName
    val symbol = tree.symbol
    val classSymbol = if (symbol.isModule) symbol.asModule.moduleClass.asClass else symbol.asClass
    val self = uniqueRealisticTermName(engine.multitierCode.symbol)
    val outer = engine.outerMultitierName
  }

  private val underExpansion: Set[Symbol] = (engine.multitierCode collect {
    case tree: DefTree => tree.symbol
  }).toSet

  def underExpansion(symbol: Symbol): Boolean = underExpansion contains symbol

  private val underEnclosingExpansion: Set[Symbol] = (engine.outerMultitierCode collect {
    case tree: DefTree => tree.symbol
  }).toSet

  def underEnclosingExpansion(symbol: Symbol): Boolean = underEnclosingExpansion contains symbol
}
