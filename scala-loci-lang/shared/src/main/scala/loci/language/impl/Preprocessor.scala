package loci
package language
package impl

import scala.language.higherKinds
import scala.reflect.macros.blackbox

trait Preprocessor[C <: blackbox.Context] {
  val c: C
  def process(tree: c.Tree): c.Tree
}

object Preprocessor {
  def run(
      c: blackbox.Context)(
      tree: c.Tree,
      factories: Seq[Preprocessor.Factory[Preprocessor]]) = {
    val preprocessors = factories.distinct map { factory => factory[c.type](c) }
    preprocessors.foldLeft(tree) { (tree, preprocessor) =>
      preprocessor process tree
    }
  }

  abstract class Factory[+Pre[C <: blackbox.Context] <: Preprocessor[C]] {
    def apply[C <: blackbox.Context](c: C): Pre[C]
  }
}
