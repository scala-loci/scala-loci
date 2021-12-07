package loci
package embedding
package impl
package preprocessors

import scala.reflect.macros.blackbox

object StatedTyping extends Preprocessor.Factory[StatedTyping] {
  def apply[C <: blackbox.Context](c: C) = new StatedTyping(c)
}

class StatedTyping[C <: blackbox.Context](val c: C) extends Preprocessor[C] {
  import c.universe._

  def process(tree: Tree): Tree = {
    tree foreach { tree =>
      if (tree.isType)
        internal.updateAttachment(tree, StatedTyping)
    }

    tree
  }
}
