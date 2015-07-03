package retier
package impl
package engine

import scala.reflect.macros.blackbox.Context

trait CodeState[C <: Context] {
  val context: C
  val tree: context.Tree
  val bases: List[context.Tree]
  val body: List[context.Tree]
  def replaceBody(body: List[context.Tree]): CodeState[C]
}
