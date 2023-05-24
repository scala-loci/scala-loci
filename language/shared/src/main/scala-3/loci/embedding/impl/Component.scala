package loci
package embedding
package impl

import scala.quoted.*

sealed trait Component:
  val quotes: Quotes
  given quotes.type = quotes

object Component:
  trait withQuotes[Q <: Quotes & Singleton](val quotes: Q) extends Component
