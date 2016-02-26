package retier
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
      tpe != null && tpe <:< types.localOn && (types.bottom forall { tpe <:!< _ })

    def isPeerDefinition(symbol: Symbol) =
      symbol != null && symbol.isClass && symbol.asClass.toType <:< types.peer

    val stats = aggregator.all[InputStatement] collect {
      case InputStatement(stat: ValOrDefDef, index)
          if isPlacedType(stat.tpt.tpe) =>
        val (peerType, exprType) = extractAndValidateType(stat, stat.tpt.tpe)
        val declTypeTree = stat.tpt.typeArgTrees.head
        PlacedStatement(stat, peerType.typeSymbol.asType, exprType,
          Some(declTypeTree), None, stat.rhs, index)

      case InputStatement(stat, index)
          if isPlacedType(stat.tpe) =>
        val (peerType, exprType) = extractAndValidateType(stat, stat.tpe)
        PlacedStatement(stat, peerType.typeSymbol.asType, exprType,
          None, None, stat, index)

      case InputStatement(stat, index)
          if !isPeerDefinition(stat.symbol) &&
             !isPeerDefinition(stat.symbol.companion) =>
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
