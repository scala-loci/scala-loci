package loci
package embedding
package impl

import scala.reflect.macros.blackbox

class Connections(val c: blackbox.Context) {
  import c.universe._

  def setup(setup: Tree): Tree = {
    val q"$_.$name[$tpt](...$_)" = c.macroApplication: @unchecked

    q"${termNames.ROOTPKG}.loci.embedding.Connections.$name(${signature(tpt)}, $setup)"
  }

  def factory(factory: Tree)(args: Tree*): Tree = {
    val q"$_.$name[$tpt](...$_)" = c.macroApplication: @unchecked
    val arguments =
      if (args.size == 1)
        args :+ q"${termNames.ROOTPKG}.scala.collection.immutable.Map.empty"
      else
        args

    q"${termNames.ROOTPKG}.loci.embedding.Connections.$name(..${signature(tpt) +: factory +: arguments})"
  }

  private val documentationCompiler =
    c.compilerSettings.size > 1 && (c.compilerSettings sliding 2 exists {
      case Seq(flag, value) =>
        flag == "-d" && ((value endsWith "/api") || (value endsWith "\\api"))
      case _ =>
        false
    })

  private def signature(tpt: Tree) = {
    val name = TermName(s"$$loci$$peer$$sig$$${tpt.symbol.name}")

    tpt match {
      case _ if documentationCompiler || c.hasErrors =>
        q"${termNames.ROOTPKG}.scala.Predef.???"

      case tq"$prefix.$_" if (prefix.tpe member name) != NoSymbol =>
        q"$prefix.$name"

      case _ =>
        c.abort(
          if (tpt.pos != NoPosition) tpt.pos else c.enclosingPosition,
          s"$tpt is not a peer type")
    }
  }
}
