package loci.dev
package language
package impl
package components

import scala.collection.mutable
import scala.reflect.macros.blackbox

object Initialization extends Component.Factory[Initialization](
    requires = Seq(Commons, ModuleInfo)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new Initialization(engine)
  def asInstance[C <: blackbox.Context] = { case c: Initialization[C] => c }
}

class Initialization[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq(
    Phase("init:inst", instantiateNestedModules, before = Set("*")))

  val commons = engine.require(Commons)
  val moduleInfo = engine.require(ModuleInfo)

  import engine._
  import engine.c.universe._
  import commons._
  import moduleInfo._

  case class Initialized(tree: ImplDef)

  private def instantiateNestedModules(records: List[Any]): List[Any] = {
    module.tree.impl.parents foreach { parent =>
      if (!(multitierModules contains parent.symbol) &&
          !isMultitierModule(parent) &&
          parent.tpe =:!= definitions.AnyTpe &&
          parent.tpe =:!= definitions.AnyRefTpe)
        c.abort(parent.pos, "Multitier modules cannot have non-multitier parents")
    }

    object transformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case tree: ImplDef if multitierModules contains tree.symbol =>
          val annotatedTree = tree map { (_, parents, self, body) =>
            val mods = tree.mods mapAnnotations {
              _ filter { _.tpe <:!< types.multitierModule }
            }
            (mods, parents, self, body)
          }
          expandMultitierModule(annotatedTree)

        case tree =>
          super.transform(tree)
      }
    }

    records :+ Initialized(transformer transform module.tree match { case tree: ImplDef => tree })
  }

  private val multitierModules: Set[Symbol] = {
    val impls = module.tree collect { case tree: ImplDef => tree }
    val multitierModules = mutable.Set(module.symbol)

    var foundAdditionals = true
    while (foundAdditionals) {
      val additionals = (impls collect {
        case impl if isMultitierModule(impl) || (impl.impl.parents exists isMultitierModule) =>
          impl.symbol
      }).toSet -- multitierModules

      if (additionals.isEmpty)
        foundAdditionals = false
      else
        multitierModules ++= additionals
    }

    multitierModules.toSet - module.symbol
  }

  private def isMultitierModule(tree: Tree): Boolean =
    tree.symbol.allAnnotations exists { _.tree.tpe <:< types.multitierModule }
}
