package loci
package impl
package engine

import scala.reflect.macros.blackbox.Context

object Echo {
  def apply[C <: Context](c: C): Echo[c.type] =
    new Echo[c.type](c)
}

class Echo[C <: Context](protected val c: C) {
  import c.universe._

  val verboseEcho =
    (c.compilerSettings contains "-verbose") ||
    (c.settings contains "loci:macro:vebose")

  def apply(verbose: Boolean, msg: String): Unit =
    if (!verbose || verboseEcho)
      c.echo(NoPosition, msg)
}
