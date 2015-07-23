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

    val peers = (aggregator.all[PeerDefinition] map { _.peer }).toSet

    def extractAndValidateType(tree: Tree, tpe: Type) = {
      val Seq(exprType, peerType) = tpe.typeArgs

      if (!(peers contains peerType))
        c.abort(tree.pos,
          "Placed abstractions must be placed on a peer " +
          "that is defined in the same scope")

      (peerType, exprType)
    }

    def extractTypeTree(tree: Tree) = {
      val args = tree match {
        case tree: TypeTree if tree.original != EmptyTree =>
          tree.original match {
            case AppliedTypeTree(_, args) => args
            case _ => List(EmptyTree)
          }
        case AppliedTypeTree(_, args) => args
        case _ => List(EmptyTree)
      }

      args.head match {
        case tree: TypeTree if tree.original != EmptyTree => tree.original
        case tree => tree
      }
    }

    val stats = aggregator.all[InputStatement] map { _.stat } collect {
      case stat: ValOrDefDef if stat.tpt.tpe <:< types.localOn =>
        val (peerType, exprType) = extractAndValidateType(stat, stat.tpt.tpe)
        val declTypeTree = extractTypeTree(stat.tpt) orElse stat.tpt
        PlacedStatement(
          stat, peerType, exprType, Some(declTypeTree), None, stat.rhs)

      case stat if stat.tpe <:< types.localOn =>
        val (peerType, exprType) = extractAndValidateType(stat, stat.tpe)
        PlacedStatement(
          stat, peerType, exprType, None, None, stat)

      case stat =>
        NonPlacedStatement(stat)
    }

    val placedStats = stats collect { case stat: PlacedStatement => stat }
    val nonPlacedStats = stats collect { case stat: NonPlacedStatement => stat }

    echo(
      verbose = true,
      s"Collected placed statements " +
      s"(${placedStats.size} placed statements added)")

    echo(
      verbose = true,
      s"Collected non-placed statements " +
      s"(${nonPlacedStats.size} non-placed statements added)")

    aggregator add placedStats add nonPlacedStats
  }
}
