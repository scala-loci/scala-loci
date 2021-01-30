package loci

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object CompileTimeUtils {
  def replace(value: String, from: String, to: String): String =
    macro replaceImpl

  def replaceImpl(c: whitebox.Context)(value: c.Tree, from: c.Tree, to: c.Tree): c.Tree = {
    import c.universe._

    (value, from, to) match {
      case (Literal(Constant(value: String)), Literal(Constant(from: String)), Literal(Constant(to: String))) =>
        Literal(Constant(value.replace(from, to)))
      case _ =>
        c.abort(c.enclosingPosition, "string literal expected")
    }
  }
}
