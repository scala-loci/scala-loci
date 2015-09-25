package retier
package impl
package engine.generators

import engine._
import scala.collection.mutable.ListBuffer
import scala.reflect.NameTransformer
import scala.reflect.macros.blackbox.Context

trait RemoteExpressionProcessor { this: Generation =>
  val c: Context
  import c.universe._
  import names._

  val processRemoteExpressions = UniformAggregation[
    EnclosingContext with PeerDefinition with PlacedStatement] {
      aggregator =>

    echo(verbose = true, " Processing remote expressions")

    val enclosingName = aggregator.all[EnclosingContext].head.name
    val peerTypes = aggregator.all[PeerDefinition] map { _.peerType }
    val declStats = ListBuffer.empty[PlacedStatement]

    val stats = aggregator.all[PlacedStatement] map { stat =>
      val defs = (stat.expr collect { case tree: DefTree => tree.symbol }).toSet
      stat.copy(expr =
        new RemoteExpressionGenerator(defs, enclosingName, peerTypes, declStats)
          transform stat.expr)
    }

    echo(verbose = true,
      s"  [${stats.size} placed statements generated, existing replaced]")
    echo(verbose = true,
      s"  [${declStats.size} placed statements added]")

    aggregator replace stats add declStats
  }

  private class RemoteExpressionGenerator(defs: Set[Symbol],
    enclosingName: TypeName, peerTypes: List[Type],
    declStats: ListBuffer[PlacedStatement])
      extends Transformer {
    def processSelectionExpression(expr: Tree, selected: Tree) = {
      val (remoteExpr, selectionTree) = expr match {
        case tree @ q"$expr.$_[..$_](...$exprss)"
            if symbols.remoteOn contains tree.symbol =>
          (expr, Some(exprss.head))
        case _ =>
          (expr, None)
      }

      val q"$_[$tpt]" = remoteExpr
      val typeTree = markRetierSynthetic(
        tpt.typeTree(abortOnFailure = true),
        remoteExpr.pos)

      val fromExpression = markRetierSynthetic(
        internal setSymbol (q"`<FromExpressionDummy>`", symbols.fromExpression),
        remoteExpr.pos)

      selectionTree match {
        case Some(selectionTree) =>
          q"$fromExpression($selected) $from[$typeTree] (..$selectionTree)"
        case _ =>
          markRetierSynthetic(selected, recursive = false)
      }
    }

    override def transform(tree: Tree) = tree match {
      case q"$expr.$_[..$_](...$exprss)"
          if symbols.remoteCall == tree.symbol =>
        val call = super.transform(exprss.head.head)
        processSelectionExpression(expr, call)

      case q"$expr.$_[..$_](...$exprssIdentifier).$_[..$_](...$exprssValue)"
          if symbols.remoteSet == tree.symbol =>
        val value = exprssValue.head.head
        val identifier = exprssIdentifier.head.head

        val setter = identifier match {
          case q"$expr.$tname" =>
            if (!identifier.symbol.isTerm ||
                !identifier.symbol.asTerm.isGetter ||
                !identifier.symbol.asTerm.accessed.asTerm.isVar)
              c.abort(identifier.pos, "variable expected")

            val name = TermName(NameTransformer encode s"${tname.toString}_=")
            q"$expr.$name"

          case _ =>
            c.abort(identifier.pos, "identifier expected")
        }

        val TypeRef(pre, sym, List(_, peerType)) =
          identifier.tpe.widen.dealias
        val tpe =
          internal typeRef (pre, sym, List(definitions.UnitTpe, peerType))

        val call = super.transform(internal setType (q"$setter($value)", tpe))
        processSelectionExpression(expr, call)

      case q"$_[..$_](...$_)"
          if symbols.remote contains tree.symbol =>
        val (exprBase, values, expr) = tree match {
          case q"$expr.$_[..$_].$_[..$_](...$values).$_[..$_](...$exprss)"
              if symbols.remoteIssuedUsingIn contains tree.symbol =>
            (expr, values.head, exprss.head.head)
          case q"$expr.$_[..$_](...$values).$_[..$_](...$exprss)"
              if symbols.remoteUsingIn contains tree.symbol =>
            (expr, values.head, exprss.head.head)
          case q"$expr.$_[..$_].$_[..$_](...$exprss)"
              if symbols.remoteIssuedApply == tree.symbol =>
            (expr, List.empty, exprss.head.head)
          case q"$expr.$_[..$_](...$exprss)" =>
            (expr, List.empty, exprss.head.head)
        }

        val (remoteExpr, args) =
          if (values.isEmpty) {
            val q"(..$_) => $exprRemote" = expr
            (exprRemote, List.empty)
          }
          else {
            val q"(..$_) => (..$args) => $exprRemote" = expr
            (exprRemote, args)
          }

        val localDefs = (expr collect {
          case tree: DefTree => tree.symbol
        }).toSet
        val refs = expr collect {
          case tree: RefTree => tree.symbol -> tree.pos
        }

        refs foreach { case (ref, pos) =>
          if ((defs contains ref) && !(localDefs contains ref))
            c.abort(pos,
              "remote value is not locally available " +
              "(remote values can be transferred via `using in` clause)")
        }

        val Seq(exprType, peerType) = tree.tpe.typeArgs
        val exprTypeTree =
          internal setType (typer createTypeTree exprType, exprType)

        val TypeRef(pre, sym, _) = types.sharedOn.typeSymbol.asType.toType
        val declType = internal typeRef (pre, sym, List(exprType, peerType))
        val declTypeTree =
          internal setType (typer createTypeTree declType, declType)

        if (!(peerTypes exists { peerType <:< _ }))
          c.abort(tree.pos,
            "remote expressions must be assigned to a peer " +
            "that is defined in the same scope" + peerType + peerTypes)

        // handle issued types
        val processedRemoteExpr =
          if (exprType <:< types.issuedControlled &&
              exprType <:!< types.issued &&
              (types.functionPlacing exists { remoteExpr.tpe <:< _ }))
            q"""${markRetierSynthetic(trees.ControlledIssuedValueCreate)}[
                ..${exprType.typeArgs}]($remoteExpr)"""
          else if (exprType <:< types.issuedControlled &&
                   (types.issuedPlacing forall { remoteExpr.tpe <:!< _ }))
            q"""${markRetierSynthetic(trees.IssuedValueCreate)}[
                ..${exprType.typeArgs}]($remoteExpr)"""
          else
            remoteExpr

        val Some(collectedPeerType) = peerTypes collectFirst {
          case tpe if tpe <:< peerType => tpe
        }

        val count = declStats count { _.peerType == collectedPeerType }
        val peerName = peerType.typeSymbol.asType.name
        val name = retierTermName(s"anonymous$$$peerName$$$count")

        val dummyDefinition = markRetierSynthetic(
          q"def $name(..$args): $declTypeTree = `<expressionDummy>`")

        declStats += PlacedStatement(
          dummyDefinition, collectedPeerType, exprType,
          Some(markRetierSynthetic(exprTypeTree)), None, processedRemoteExpr)

        val call = super.transform(
          internal setType (q"$enclosingName.this.$name(..$values)", declType))
        processSelectionExpression(exprBase, call)

      case _ =>
        super.transform(tree)
    }
  }
}
