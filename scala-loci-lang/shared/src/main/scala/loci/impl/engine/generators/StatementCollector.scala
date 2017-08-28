package loci
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait StatementCollector { this: Generation =>
  val c: Context
  import c.universe._

  val collectStatements = AugmentedAggregation[
    InputStatement with PeerDefinition,
    PlacedStatement with NonPlacedStatement] {
      aggregator =>

    echo(verbose = true, " Collecting placed and non-placed statements")

    val peerSymbols = aggregator.all[PeerDefinition] map { _.peerSymbol }

    def extractAndValidateType(tree: Tree, tpe: Type) = {
      val Seq(exprType, peerType) = tpe.underlying.typeArgs

      if (!(peerSymbols contains peerType.typeSymbol))
        c.abort(tree.pos,
          "placed abstractions must be placed on a peer " +
          "that is defined in the same scope")

      (peerType, exprType)
    }

    def isPlacedType(tpe: Type) =
      tpe <:< types.localOn && (types.bottom forall { tpe <:!< _ })

    def isPeerDefinition(symbol: Symbol) =
      symbol.isClass && symbol.asClass.toType <:< types.peer

    def isPlacedExpression(tree: Tree) = {
      val expr =
        if (symbols.globalCasts contains tree.symbol) {
          val q"$_(...$exprss)" = tree
          exprss.head.head
        }
        else
          tree

      symbols.placed contains expr.symbol
    }

    def isTypeGiven(tree: Tree) = tree match {
      case tree: TypeTree => tree.original != null
      case _ => true
    }

    val stats = aggregator.all[InputStatement] collect {
      case InputStatement(stat: ValOrDefDef, index)
          if stat.tpe != null && isPlacedType(stat.tpt.tpe) &&
             (isPlacedExpression(stat.rhs) || isTypeGiven(stat.tpt)) =>
        val (peerType, exprType) = extractAndValidateType(stat, stat.tpt.tpe)
        val declTypeTree = stat.tpt.typeArgTrees.head
        PlacedStatement(stat, peerType.typeSymbol.asType, exprType,
          Some(declTypeTree), None, stat.rhs, index)

      case InputStatement(stat, index) if isPlacedExpression(stat) =>
        val (peerType, exprType) = extractAndValidateType(stat, stat.tpe)
        PlacedStatement(stat, peerType.typeSymbol.asType, exprType,
          None, None, stat, index)

      case InputStatement(stat, index)
          if stat.symbol == null ||
             (!isPeerDefinition(stat.symbol) &&
              !isPeerDefinition(stat.symbol.companion)) =>
        NonPlacedStatement(stat, index)
    }

    val placedStats = stats collect { case stat: PlacedStatement => stat }
    val nonPlacedStats = stats collect { case stat: NonPlacedStatement => stat }

    echo(verbose = true,
      s"  [${placedStats.size} placed statements added]")
    echo(verbose = true,
      s"  [${nonPlacedStats.size} non-placed statements added]")

    aggregator add placedStats add nonPlacedStats
  }
}
