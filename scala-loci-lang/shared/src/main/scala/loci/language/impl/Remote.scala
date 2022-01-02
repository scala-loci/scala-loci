package loci
package language
package impl

import scala.reflect.macros.blackbox

object Remote {
  def asRemote[P: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._

    val q"$expr.asRemote[$tpt]" = c.macroApplication: @unchecked

    val enclosingMultitierMacro = c.enclosingMacros exists { c =>
      import c.universe._

      val multitier = c.mirror.staticClass(s"${termNames.ROOTPKG}.loci.multitier")

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

    val documentationCompiler = c.compilerSettings sliding 2 exists {
      case Seq(flag, value) =>
        flag == "-d" && ((value endsWith "/api") || (value endsWith "\\api"))
      case _ =>
        false
    }

    if (!enclosingMultitierMacro && !documentationCompiler) {
      val remote = weakTypeOf[Remote[P]]
      val reference = typeOf[runtime.Remote.Reference]
      val selfReference = weakTypeOf[runtime.Remote.SelfReference[P]]
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
        case $ref: $selfReference if $ref.signature <= $signature =>
          ${termNames.ROOTPKG}.scala.Some[$remote]($ref)
        case _ =>
          ${termNames.ROOTPKG}.scala.None
      }"""
    }
    else if (documentationCompiler)
      atPos(tpt.pos) { q"${termNames.ROOTPKG}.scala.Predef.???" }
    else
      atPos(tpt.pos) { q"${termNames.ROOTPKG}.loci.runtime.Remote.cast[$tpt]($expr)" }
  }
}
