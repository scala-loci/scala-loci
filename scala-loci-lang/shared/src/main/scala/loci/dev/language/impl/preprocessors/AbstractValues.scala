package loci.dev
package language
package impl
package preprocessors

import scala.reflect.macros.blackbox

object AbstractValues extends Preprocessor.Factory[AbstractValues] {
  def apply[C <: blackbox.Context](c: C) = new AbstractValues(c)
}

class AbstractValues[C <: blackbox.Context](val c: C) extends Preprocessor[C] {
  import c.universe._

  def process(tree: Tree): Tree = {
    def reducedFlags(mods: Modifiers) =
      Seq(Flag.ABSOVERRIDE, Flag.ARTIFACT, Flag.FINAL, Flag.IMPLICIT,
          Flag.LOCAL, Flag.MUTABLE, Flag.OVERRIDE, Flag.PRESUPER,
          Flag.PRIVATE, Flag.PROTECTED, Flag.STABLE, Flag.SYNTHETIC)
        .foldLeft(NoFlags) { (flagAcc, flag) =>
          if (mods hasFlag flag) flagAcc | flag else flagAcc
        }

    def processMods(mods: Modifiers) =
      Modifiers(reducedFlags(mods), mods.privateWithin, annotation :: mods.annotations)

    def processStats(stats: List[Tree]) = stats map {
      case tree @ DefDef(mods, name, tparams, vparamss, tpt, EmptyTree)
          if (mods hasFlag Flag.DEFERRED) && !(mods hasFlag Flag.PRIVATE) =>
        treeCopy.DefDef(
          tree, processMods(mods), name, tparams, vparamss, tpt, q"null.asInstanceOf[$tpt]")

      case tree @ ValDef(mods, name, tpt, EmptyTree)
          if (mods hasFlag Flag.DEFERRED) && !(mods hasFlag Flag.PRIVATE) =>
        treeCopy.ValDef(
          tree, processMods(mods), name, tpt, q"null.asInstanceOf[$tpt]")

      case tree =>
        tree
    }

    object transformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case ClassDef(mods, name, tparams, impl @ Template(parents, self, body))
            if mods hasFlag Flag.FINAL =>
          super.transform(treeCopy.ClassDef(
            tree, mods, name, tparams, treeCopy.Template(
              impl, parents, self, processStats(body))))

        case ModuleDef(mods, name, impl @ Template(parents, self, body)) =>
          super.transform(treeCopy.ModuleDef(
            tree, mods, name, treeCopy.Template(
              impl, parents, self, processStats(body))))

        case _ =>
          super.transform(tree)
      }
    }

    transformer transform tree
  }

  private val annotation = q"new ${termNames.ROOTPKG}.loci.dev.runtime.AbstractValue"
}
