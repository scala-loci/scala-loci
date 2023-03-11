package loci
package embedding
package impl
package components

trait ErrorReporter:
  this: Component =>
  import quotes.reflect.*

  private var errors = List.empty[(String, Position)]

  def errorAndCancel(msg: String, pos: Position): Unit =
    errors ::= (msg, pos)

  def reportErrors() =
    val reversed = errors.reverse
    val filtered = reversed filterNot { (_, pos) => pos == Position.ofMacroExpansion }
    if filtered.nonEmpty then
      filtered foreach { report.error(_, _) }
    else
      reversed foreach { report.error(_, _) }

  def canceled = errors.nonEmpty
end ErrorReporter
