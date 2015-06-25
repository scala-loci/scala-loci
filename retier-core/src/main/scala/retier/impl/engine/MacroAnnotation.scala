package retier
package impl
package engine

import scala.reflect.macros.whitebox.Context

object multitier {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val util = Util(c)
    val annottee :: _ = annottees map { _.tree }

    val result = (util typecheck annottee) match {
      case classDef @ q"""
          $mods class $tpname[..$tparams] $ctorMods(...$paramss)
            extends { ..$earlydefns } with ..$parents { $self =>
            ..$stats
          }""" =>

        classDef

      case _ =>
        c.abort(c.enclosingPosition, "`multitier` macro only applicable to classes")
        EmptyTree
    }
    c.Expr[Any](result)
  }
}
