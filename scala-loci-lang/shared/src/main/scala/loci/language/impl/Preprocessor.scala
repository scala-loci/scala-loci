package loci
package language
package impl

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
    val logging = Logging(c)

    val preprocessors = factories.distinct map { factory => factory[c.type](c) }

    logging.debug("Multitier preprocessors")
    preprocessors foreach { preprocessor =>
      logging.debug(s" ${name(preprocessor)}")
    }

    preprocessors.foldLeft(tree) { (tree, preprocessor) =>
      logging.debug(s"Running multitier preprocessor ${name(preprocessor)}")
      preprocessor process tree
    }
  }

  private def name(ref: AnyRef) = {
    val name = ref.getClass.getSimpleName
    if (name endsWith "$")
      name.dropRight(1)
    else
      name
  }

  abstract class Factory[+Pre[C <: blackbox.Context] <: Preprocessor[C]] {
    def apply[C <: blackbox.Context](c: C): Pre[C]
  }
}
