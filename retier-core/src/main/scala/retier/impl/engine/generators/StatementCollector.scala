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

    val placedStats = aggregator.all[InputStatement] map { _.stat } collect {
      case stat @ ValDef(mods, name, tpt, rhs)
          if tpt.tpe <:< types.localOn =>
        val (peerType, exprType) = extractAndValidateType(stat, tpt.tpe)
        val decl = ValDef(mods, name, EmptyTree, EmptyTree)
        internal setPos (decl, stat.pos)
        PlacedStatement(stat, peerType, exprType, Some(decl), None, rhs)

      case stat @ DefDef(mods, name, tparams, vparamss, tpt, rhs)
          if tpt.tpe <:< types.localOn =>
        val (peerType, exprType) = extractAndValidateType(stat, tpt.tpe)
        val decl = DefDef(mods, name, tparams, vparamss, EmptyTree, EmptyTree)
        internal setPos (decl, stat.pos)
        PlacedStatement(stat, peerType, exprType, Some(decl), None, rhs)

      case stat
          if stat.tpe <:< types.localOn &&
             (symbols.placed contains stat.symbol) =>
        val (peerType, exprType) = extractAndValidateType(stat, stat.tpe)
        PlacedStatement(stat, peerType, exprType, None, None, stat)
    }

    val nonPlacedStats = aggregator.all[InputStatement] map { _.stat } collect {
      case stat
          if !stat.symbol.isClass ||
             !(stat.symbol.asClass.toType <:< types.peer) =>
        NonPlacedStatement(stat)
    }

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
