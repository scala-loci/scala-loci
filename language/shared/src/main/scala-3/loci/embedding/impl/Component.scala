package loci
package embedding
package impl

import scala.quoted.*

trait Component:
  val quotes: Quotes
  given quotes.type = quotes
