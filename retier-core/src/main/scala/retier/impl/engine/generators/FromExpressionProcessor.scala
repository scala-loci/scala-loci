package retier
package impl
package engine.generators

import engine._
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox.Context

trait FromExpressionProcessor { this: Generation =>
  val c: Context
  import c.universe._
  import names._
  import trees._

  val processFromExpressions = UniformAggregation[
    PeerDefinition with PlacedStatement] {
      aggregator =>

    echo(verbose = true, " Processing from expressions")

    val peerTypes = aggregator.all[PeerDefinition] map { _.peerType }

    val stats = aggregator.all[PlacedStatement] map { stat =>
      stat.copy(expr =
        new FromExpressioProcessor(peerTypes) transform stat.expr)
    }

    echo(verbose = true,
      s"  [${stats.size} placed statements generated, existing replaced]")

    aggregator replace stats
  }

  private class FromExpressioProcessor(peerTypes: List[Type])
      extends Transformer {
    def processFromExpression(exprssValue: List[List[Tree]], tpt: Tree,
        exprssPeer: List[List[Tree]], isRetierSynthetic: Boolean) = {
      val value = exprssValue.head.head

      val Seq(_, peerType) = value.tpe.widen.typeArgs
      val interface = peerInterfaceTree(value, peerType, peerTypes)

      val typeTree = tpt.typeTree(abortOnFailure = true)
      val typeTag = peerTypeTagTree(typeTree, tpt.tpe, peerTypes)

      val messageUnexpectedTree = "identifier expected"
      val messageUnexpectedMethodTree = messageUnexpectedTree +
        " (note that method invocations require a `remote call` expression)"

      val transmissionProperties = value match {
        case q"$_.$tname" =>
          q"$interface.$tname"

        case q"$_.$tname[..$tpts](...$exprss)" =>
          if (isRetierSynthetic)
            q"$interface.$tname[..$tpts](...$exprss)"
          else
            c.abort(value.pos, messageUnexpectedMethodTree)

        case _ =>
          c.abort(value.pos, messageUnexpectedTree)
      }

      val args =
        markRetierSynthetic(transmissionProperties) +:
        (exprssPeer.headOption.toList flatMap {
          _ map { remote =>
            val remoteType = markRetierSynthetic(tq"$Remote[$typeTree]")
            q"$remote: $remoteType"
          }
        })

      val peerTypeTag = markRetierSynthetic(typeTag)

      val tpe =
        if (exprssPeer.isEmpty || exprssPeer.head.isEmpty)
          types.from
        else if (exprssPeer.head.size == 1)
          types.fromSingle
        else
          types.fromMultiple

      internal setType (
        q"$system.createPeerSelection(..$args)($peerTypeTag)",
        tpe)
    }

    override def transform(tree: Tree) = tree match {
      case q"(new $expr[..$_](...$exprssValue)).$tname[$tpt](...$exprssPeer)"
          if expr.tpe <:< types.fromExpression &&
             tname.encodedName == names.from.encodedName =>
        super.transform(
          processFromExpression(
            exprssValue, tpt, exprssPeer, expr.isRetierSynthetic))

      case q"$expr[..$_](...$exprssValue).$tname[$tpt](...$exprssPeer)"
          if expr.symbol == symbols.fromExpression &&
             tname.encodedName == names.from.encodedName =>
        super.transform(
          processFromExpression(
            exprssValue, tpt, exprssPeer, expr.isRetierSynthetic))

      case _ =>
        super.transform(tree)
    }
  }
}
