package loci
package language
package impl

import scala.reflect.macros.blackbox

object Logging {
  def apply(c: blackbox.Context) = new Logging(c)
}

class Logging(c: blackbox.Context) {
  private val info = c.compilerSettings contains "-verbose"
  private val debug = c.settings contains "loci.macro.verbose"
  private val code = c.settings contains "loci.macro.expanded-code"

  def infoEnabled = info || debug

  def debugEnabled = debug

  def codeEnabled = code

  def info(message: => String): Unit =
    if (infoEnabled)
      c.info(c.universe.NoPosition, message, force = true)

  def debug(message: => String): Unit =
    if (debugEnabled)
      c.info(c.universe.NoPosition, message, force = true)

  def code(message: => String): Unit =
    if (codeEnabled)
      c.info(c.universe.NoPosition, message, force = true)
}
