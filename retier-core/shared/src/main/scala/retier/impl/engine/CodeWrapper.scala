package retier
package impl
package engine

import scala.reflect.macros.blackbox.Context

trait CodeWrapper[C <: Context] {
  val context: C
  val tree: context.Tree
  val name: context.TypeName
  val bases: List[context.Tree]
  val body: List[context.Tree]
  def replaceBody(body: List[context.Tree]): CodeWrapper[C]
  def typechecked: CodeWrapper[C]
  def untypechecked: CodeWrapper[C]
}
