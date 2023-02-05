package loci
package embedding
package impl

import scala.quoted.*

trait ErrorReporter:
  this: Component =>
  import quotes.reflect.*

  private var errorAndCanceled = false

  def errorAndCancel(msg: String, pos: Position): Unit =
    report.error(msg, pos)
    errorAndCanceled = true

  def canceled = errorAndCanceled
end ErrorReporter
