package loci.language.impl

import scala.reflect.macros.blackbox

object ValueRef {
  def at[V: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._

    val q"$expr.at[$tpt]" = c.macroApplication: @unchecked
    val valueType = weakTypeOf[V]

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

    if (documentationCompiler) {
      atPos(tpt.pos) { q"${termNames.ROOTPKG}.scala.Predef.???" }
    } else if (enclosingMultitierMacro) {
      atPos(tpt.pos) { q"${termNames.ROOTPKG}.loci.valueref.ValueRef.cast[$valueType, $tpt]($expr)" }
    } else {
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

      val ref = TermName("$loci$value$ref")

      q"""$expr match {
         case $ref if $ref.signature <= $signature => ${termNames.ROOTPKG}.scala.Some($ref.copy[$valueType, $tpt]())
         case _ => ${termNames.ROOTPKG}.scala.None
       }"""
    }
  }
}
