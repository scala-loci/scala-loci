package loci.dev
package language
package impl

import retypecheck._

import scala.reflect.macros.blackbox.Context

trait MultitierCode {
  val c: Context

  import c.universe._

  private val treeCopier = newLazyTreeCopier

  trait MultitierCode {
    val tree: ImplDef
    val bases: List[Tree]
    val body: List[Tree]
    def replaceBody(body: List[Tree]): MultitierCode
    def typechecked: MultitierCode
    def untypechecked: MultitierCode
  }

  class MultitierClass(impl: Tree) extends MultitierCode {
    val tree @ ClassDef(mods, tpname, tparams, Template(bases, self, body)) = impl

    def replaceBody(body: List[Tree]) = new MultitierClass(
      treeCopier.ClassDef(tree, mods, tpname, tparams, Template(bases, self, body)))

    def typechecked = new MultitierClass(c.retyper retypecheckAll tree)
    def untypechecked = new MultitierClass(c.retyper untypecheckAll tree)
  }

  class MultitierModule(impl: Tree) extends MultitierCode {
    val tree @ ModuleDef(mods, tname, Template(bases, self, body)) = impl

    def replaceBody(body: List[Tree]) = new MultitierModule(
      treeCopier.ModuleDef(tree, mods, tname, Template(bases, self, body)))

    def typechecked = new MultitierModule(c.retyper retypecheckAll tree)
    def untypechecked = new MultitierModule(c.retyper untypecheckAll tree)
  }
}
