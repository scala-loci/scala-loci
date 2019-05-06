package loci.dev
package language
package impl

import scala.reflect.macros.blackbox

object Remote {
  def asRemote[P: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._

    val q"$expr.asRemote[$tpt]" = c.macroApplication

    val enclosingMultitierMacro = c.enclosingMacros exists { c =>
      import c.universe._

      val multitier = c.mirror.staticClass(s"${termNames.ROOTPKG}.loci.dev.multitier")

      c.macroApplication match {
        case q"new $expr[..$tpts](...$exprss).macroTransform[..$_](...$_)" =>
          c.typecheck(q"new $expr[..$tpts](...$exprss)", silent = true) match {
            case q"new $expr[..$_](...$_)" =>
              expr.symbol == multitier
            case _ =>
              false
          }
        case _ =>
          false
      }
    }

    if (!enclosingMultitierMacro) {
      val remote = weakTypeOf[Remote[P]]
      val reference = typeOf[runtime.Remote.Reference]
      val name = TermName(s"$$loci$$peer$$sig$$${tpt.symbol.name}")

      val prefix = tpt match {
        case tq"$prefix.$_" if (prefix.tpe member name) != NoSymbol =>
          prefix
        case _ =>
          c.abort(
            if (tpt.pos != NoPosition) tpt.pos else c.enclosingPosition,
            s"$tpt is not a peer type")
      }

      val signature = q"$prefix.$name"
      val ref = TermName("$loci$ref")

      q"""$expr match {
        case $ref: $reference if $ref.signature <= $signature =>
          ${termNames.ROOTPKG}.scala.Some[$remote]($ref)
        case _ =>
          ${termNames.ROOTPKG}.scala.None
      }"""
    }
    else
      atPos(tpt.pos) { q"${termNames.ROOTPKG}.loci.dev.runtime.Remote.cast[$tpt]($expr)" }
  }
}
