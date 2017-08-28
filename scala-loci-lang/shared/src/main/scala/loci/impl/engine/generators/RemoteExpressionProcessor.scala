package loci
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
    def processSelectionExpression(tree: Tree, selected: Tree) = {
      val (remoteExpr, selectionTree) = tree match {
        case q"$expr.$_[..$_](...$exprss)"
            if symbols.remoteOn contains tree.symbol =>
          (expr, Some(exprss.head))
        case _ =>
          (tree, None)
      }

      val q"$_[$tpt]" = remoteExpr
      val typeTree = markLociSynthetic(
        tpt.typeTree(abortOnFailure = true),
        remoteExpr.pos)

      val fromExpression = markLociSynthetic(
        internal setSymbol (q"`<FromExpressionDummy>`", symbols.fromExpression),
        remoteExpr.pos)

      selectionTree match {
        case Some(selectionTree) =>
          q"$fromExpression($selected) $from[$typeTree] (..$selectionTree)"
        case _ =>
          markLociSynthetic(selected, recursive = false)
      }
    }

    def processTypedWrapper(tree: Tree, tpe: Option[Type]) = tpe match {
      case Some(tpe) =>
        val remoteExpression = markLociSynthetic(
          internal setSymbol (
            q"`<RemoteExpressionDummy>`",
            symbols.remoteApply),
          tree.pos)

        internal setType (q"$remoteExpression($tree)", tpe)

      case _ =>
        tree
    }

    def process(tree: Tree, wrapperType: Option[Type]) = tree match {
      case q"$expr.$_[..$_](...$exprss)"
          if symbols.remoteCall == tree.symbol =>
        val call = super.transform(exprss.head.head)
        processTypedWrapper(
          processSelectionExpression(expr, call), wrapperType)

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
          identifier.tpe.underlying.dealias
        val tpe =
          internal typeRef (pre, sym, List(definitions.UnitTpe, peerType))

        val call = super.transform(internal setType (q"$setter($value)", tpe))
        processTypedWrapper(
          processSelectionExpression(expr, call), wrapperType)

      case q"$_[..$_](...$_)"
          if symbols.remote contains tree.symbol =>
        // decompose tree
        val (exprBase, args, expr) = tree match {
          case q"$expr.$_[..$_].$_[..$_](...$exprssArg).$_[..$_](...$exprss)"
              if tree.symbol == symbols.remoteSubjectiveCaptureApply =>
            (expr, exprssArg.head, exprss.head.head)
          case q"$expr.$_[..$_](...$exprssArg).$_[..$_](...$exprss)"
              if tree.symbol == symbols.remoteCaptureApply =>
            (expr, exprssArg.head, exprss.head.head)
          case q"$expr.$_[..$_].$_[..$_](...$exprss)"
              if tree.symbol == symbols.remoteSubjectiveApply =>
            (expr, List.empty, exprss.head.head)
          case q"$expr.$_[..$_](...$exprss)" =>
            (expr, List.empty, exprss.head.head)
        }

        // decompose types
        val Seq(originalExprType, peerType) = tree.tpe.underlying.typeArgs
        val exprType =
          if (wrapperType.isEmpty)
            originalExprType
          else if ((originalExprType <:< types.subjective ||
                    originalExprType <:< types.subjectiveControlled) &&
                   (types.bottom forall { originalExprType <:!< _ })) {
            val TypeRef(pre, sym, Seq(peerType, _)) = originalExprType.underlying
            internal typeRef (pre, sym, List(peerType, definitions.UnitTpe))
          }
          else
            definitions.UnitTpe
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
            val typeTree =
              if ((arg.tpe <:< types.localOn) &&
                  (types.bottom forall { arg.tpe <:!< _ })) {
                val Seq(placedArg, peer) = arg.typeArgTrees

                if (peer.tpe.typeSymbol == peerType.typeSymbol)
                  c.warning(arg.pos, "captured value shadows placed value")

                if ((types.subjectivePlacing exists { placedArg.tpe <:< _ }) &&
                    (types.bottom forall { placedArg.tpe <:!< _ }))
                  placedArg.typeArgTrees.last
                else
                  placedArg
              }
              else
                arg.typeTree

            val name = lociTermName(s"arg$$$index")
            val ref = internal setType (q"$name", typeTree.tpe)
            val valDef = ValDef(
              Modifiers(Flag.PARAM), name, typeTree, EmptyTree)

            (arg.symbol, ref, valDef, arg)

          case (arg, _) =>
            c.abort(arg.pos, "identifier expected")
        }

        val declArgs = capturedArgs map { case (_, _, valDef, _) => valDef }
        val valueArgs = capturedArgs map { case (_, _, _, value) => value }

        val capturedRefs = (capturedArgs map { case (symbol, ref, _, _) =>
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
                if tree.symbol != NoSymbol && !tree.isLociSynthetic =>
              if (capturedRefs contains tree.symbol)
                internal setPos (capturedRefs(tree.symbol).duplicate, tree.pos)
              else if (localDefs contains tree.symbol)
                super.transform(tree)
              else if ((defs contains tree.symbol) ||
                       (tree.tpe <:< types.localOn &&
                        (types.bottom forall { tree.tpe <:!< _ }) &&
                        peerType.typeSymbol.asType.toType <:!<
                          tree.tpe.underlying.typeArgs.last.typeSymbol.asType.toType))
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

        // generate synthetic placed expression
        val count = declStats count { _.peerSymbol == peerType.typeSymbol }
        val peerName = peerType.typeSymbol.asType.name
        val name = lociTermName(s"anonymous$$$peerName$$$count")

        val dummyDefinition = markLociSynthetic(
          q"def $name(..$declArgs): $declTypeTree = `<expressionDummy>`")

        declStats += PlacedStatement(
          dummyDefinition, peerType.typeSymbol.asType, exprType,
          Some(markLociSynthetic(exprTypeTree)), None, remoteExpr, -1)

        val call = super.transform(
          internal setType (
            q"$enclosingName.this.$name(..$valueArgs)",
            declType))
        processTypedWrapper(
          processSelectionExpression(exprBase, call), wrapperType)

      case _ =>
        super.transform(tree)
    }

    override def transform(tree: Tree) = tree match {
      case q"$expr[..$tpts](...$exprss)"
          if symbols.transmit contains tree.symbol =>
        val transformedExprss = exprss map { _ map { process(_, None) } }
        q"$expr[..$tpts](...$transformedExprss)"
      case _ =>
        process(tree, Some(tree.tpe))
    }
  }
}
