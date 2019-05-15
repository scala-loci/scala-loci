package loci
package language
package impl
package preprocessors

import scala.reflect.macros.blackbox

object MultitierTypes extends Preprocessor.Factory[MultitierTypes] {
  def apply[C <: blackbox.Context](c: C) = new MultitierTypes(c)
}

class MultitierTypes[C <: blackbox.Context](val c: C) extends Preprocessor[C] {
  import c.universe._

  def process(tree: Tree): Tree = {
    val outer = tree

    object transformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case ClassDef(mods, name, tparams, impl @ Template(parents, self, body))
            if !(mods hasFlag Flag.TRAIT) && tree != outer =>
          super.transform(treeCopy.ClassDef(
            tree, mods, name, tparams, treeCopy.Template(
              impl, parents, self, placedValuesDef :: body)))

        case _ =>
          super.transform(tree)
      }
    }

    transformer transform tree
  }

  private val placedValuesDef =
    q"${Flag.SYNTHETIC} protected[this] def `<placed values>`: Nothing = _root_.scala.Predef.???"
}
