package retier
package impl
package engine

import scala.reflect.macros.blackbox.Context
import scala.language.higherKinds
import scala.language.existentials

object RetierTyper {
  def apply[C <: Context](c: C): RetierTyper[c.type] =
    new RetierTyper[c.type](c)
}

class RetierTyper[C <: Context](val c: C) {
  import c.universe._

  private val criticalTypes = Seq(
    typeOf[PlacingExpression[_]],
    typeOf[IssuingExpression[_, _]],
    typeOf[RemoteExpression[_, T forSome { type T[_, _] }]],
    typeOf[RemoteIssuingExpression[_, T forSome { type T[_, _] }]],
    typeOf[retier.`package`.type])

  private val criticalMethods = criticalTypes flatMap {
    _.members collect {
      case member if member.isMethod &&
          (member.asMethod.paramLists.lastOption flatMap {
            _.headOption map { _.isImplicit }
          } getOrElse false) =>
        member
    }
  }

  private object criticalMethodsLastParamListRemover extends Transformer {
    override def transform(tree: Tree) = tree match {
      case q"$tname[..$tpts](...$exprss)"
          if criticalMethods contains tree.symbol =>
        q"""$tname[..$tpts](...${
          exprss dropRight 1 map { _ map { transform(_) } }
        })"""

      case _ =>
        super.transform(tree)
    }
  }

  def dropRetierImplicitArguments(tree: Tree): Tree =
    criticalMethodsLastParamListRemover transform tree
}
