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
    val peerSymbols = aggregator.all[PeerDefinition] map { _.peerSymbol }
    val declStats = ListBuffer.empty[PlacedStatement]

    val stats = aggregator.all[PlacedStatement] map { stat =>
      val defs = (stat.expr collect { case tree: DefTree => tree.symbol }).toSet
      stat.copy(expr =
        new RemoteExpressionGenerator(defs, enclosingName, peerSymbols, declStats)
          transform stat.expr)
    }

    echo(verbose = true,
      s"  [${stats.size} placed statements generated, existing replaced]")
    echo(verbose = true,
      s"  [${declStats.size} placed statements added]")

    aggregator replace stats add declStats
  }

  private class RemoteExpressionGenerator(defs: Set[Symbol],
    enclosingName: TypeName, peerSymbols: List[Symbol],
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
        // decompose tree
        val (exprBase, exprss) = tree match {
          case q"$expr.$_[..$_].$_[..$_](...$exprss)"
              if symbols.remoteIssued contains tree.symbol =>
            (expr, exprss)
          case q"$expr.$_[..$_](...$exprss)" =>
            (expr, exprss)
        }

        val (expr, args) =
          exprss match {
            case List(values, List(expr), _) =>
              (expr, values)
            case List(List(expr), _) =>
              (expr, List.empty)
          }

        // decompose types
        val Seq(exprType, peerType) = tree.tpe.typeArgs
        val exprTypeTree =
          internal setType (typer createTypeTree exprType, exprType)

        val TypeRef(pre, sym, _) = types.sharedOn.typeSymbol.asType.toType
        val declType = internal typeRef (pre, sym, List(exprType, peerType))
        val declTypeTree =
          internal setType (typer createTypeTree declType, declType)

        if (!(peerSymbols contains peerType.typeSymbol))
          c.abort(tree.pos,
            "remote expressions must be assigned to a peer " +
            "that is defined in the same scope")

        // handle captured values
        val capturedArgs = args.zipWithIndex collect {
          case (arg @ (Select(_, _) | Ident(_)), index) =>
            val (typeTree, value) =
              if (arg.tpe <:< types.localOn) {
                val Seq(placedArg, peer) = arg.typeArgTrees

                if (peer.tpe.typeSymbol == peerType.typeSymbol)
                  c.warning(arg.pos, "captured value shadows placed value")

                val issuedArg =
                  if (types.issuedPlacing exists { placedArg.tpe <:< _ })
                    placedArg.typeArgTrees.last
                  else
                    placedArg

                val valueOp = markRetierSynthetic(
                  internal setSymbol (q"`<ValueOpDummy>`", symbols.valueOp),
                  arg.pos)

                (issuedArg, q"$valueOp($arg).value")
              }
              else
                (arg.typeTree, arg)

            val name = retierTermName(s"arg$$$index")
            val ref = internal setType (q"$name", typeTree.tpe)
            val valDef = ValDef(
              Modifiers(Flag.PARAM), name, typeTree, EmptyTree)

            (arg.symbol, ref, valDef, value)

          case (arg, _) =>
            c.abort(arg.pos, "identifier expected")
        }

        val declArgs = capturedArgs map { case (_, _, valDef, _) => valDef }
        val valueArgs = capturedArgs map { case (_, _, _, value) => value }

        val capturedDefs = (capturedArgs map { case (symbol, ref, _, _) =>
          symbol -> ref
        }).toMap

        val localDefs = (expr collect {
          case tree: DefTree if tree.symbol != NoSymbol => tree.symbol
        }).toSet

        val transmittingRefs = (expr collect {
          case tree @ q"$expr[..$_](...$exprss)"
            if (symbols.transmit contains tree.symbol) ||
               (symbols.fromExpression == expr.symbol) => exprss
          case tree @ q"new $expr[..$_](...$exprss)"
            if expr.tpe <:< types.fromExpression => exprss
        } map {
          _.headOption flatMap { _.headOption }
        } collect {
            case Some(q"$expr[..$_](...$_)") => expr
        }).toSet

        object referenceTreeProcessor extends Transformer {
          override def transform(tree: Tree) = tree match {
            case _ if transmittingRefs contains tree =>
              tree

            case tree: RefTree
                if tree.symbol != NoSymbol && !tree.isRetierSynthetic =>
              if (capturedDefs contains tree.symbol)
                internal setType (q"${capturedDefs(tree.symbol)}", tree.tpe)
              else if (localDefs contains tree.symbol)
                super.transform(tree)
              else if ((defs contains tree.symbol) ||
                       (tree.tpe <:< types.localOn &&
                        peerType.typeSymbol.asType.toType <:!<
                          tree.tpe.typeArgs.last.typeSymbol.asType.toType))
                c.abort(tree.pos,
                  "remote value is not locally available " +
                  "(remote values can be transferred via `capture` clause)")
              else
                super.transform(tree)

            case _ =>
              super.transform(tree)
          }
        }

        val q"(..$_) => $exprRemote" = expr
        val remoteExpr = referenceTreeProcessor transform transform(exprRemote)

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

        // generate synthetic placed expression
        val count = declStats count { _.peerSymbol == peerType.typeSymbol }
        val peerName = peerType.typeSymbol.asType.name
        val name = retierTermName(s"anonymous$$$peerName$$$count")

        val dummyDefinition = markRetierSynthetic(
          q"def $name(..$declArgs): $declTypeTree = `<expressionDummy>`")

        declStats += PlacedStatement(
          dummyDefinition, peerType.typeSymbol.asType, exprType,
          Some(markRetierSynthetic(exprTypeTree)), None, processedRemoteExpr)

        val call = super.transform(
          internal setType (q"$enclosingName.this.$name(..$valueArgs)", declType))
        processSelectionExpression(exprBase, call)

      case _ =>
        super.transform(tree)
    }
  }
}
