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

    echo(verbose = true, " Collected placed and non-placed statements")

    val peersTypes = (aggregator.all[PeerDefinition] map { _.peerType }).toSet

    def extractAndValidateType(tree: Tree, tpe: Type) = {
      val Seq(exprType, peerType) = tpe.typeArgs

      if (!(peersTypes contains peerType))
        c.abort(tree.pos,
          "Placed abstractions must be placed on a peer " +
          "that is defined in the same scope")

      (peerType, exprType)
    }

    def extractTypeTree(tree: Tree) = {
      val args = tree match {
        case tree: TypeTree if tree.original != null =>
          tree.original match {
            case AppliedTypeTree(_, args) => args
            case _ => List(EmptyTree)
          }
        case AppliedTypeTree(_, args) => args
        case _ => List(EmptyTree)
      }

      args.head match {
        case tree: TypeTree if tree.original != null => tree.original
        case tree => tree
      }
    }

    def isPlacedType(tpe: Type) =
      tpe <:< types.localOn && (types.bottom forall { tpe <:!< _ })

    def isPeerDefinition(symbol: Symbol) =
      symbol != null && symbol.isClass && symbol.asClass.toType <:< types.peer

    val stats = aggregator.all[InputStatement] map { _.stat } collect {
      case stat: ValOrDefDef if isPlacedType(stat.tpt.tpe) =>
        val (peerType, exprType) = extractAndValidateType(stat, stat.tpt.tpe)
        val declTypeTree = extractTypeTree(stat.tpt) orElse TypeTree(exprType)
        PlacedStatement(
          stat, peerType, exprType, Some(declTypeTree), None, stat.rhs)

      case stat if isPlacedType(stat.tpe) =>
        val (peerType, exprType) = extractAndValidateType(stat, stat.tpe)
        PlacedStatement(
          stat, peerType, exprType, None, None, stat)

      case stat
          if !isPeerDefinition(stat.symbol) &&
             !isPeerDefinition(stat.symbol.companion) =>
        NonPlacedStatement(stat)
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
