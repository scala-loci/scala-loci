package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait OutputGenerator { this: Generation =>
  val c: Context
  import c.universe._
  import trees._

  val generateOutput = AugmentedAggregation[
    NonPlacedStatement with PlacedStatement with PeerDefinition,
    OutputStatement] {
      aggregator =>

    echo(verbose = true, " Generating output")

    val compileTimeOnlyAnnotation =
      q"""new $compileTimeOnly("Only usable in `multitier` environment")"""

    def annotate(mods: Modifiers) =
      Modifiers(
        mods.flags, mods.privateWithin,
        compileTimeOnlyAnnotation +: mods.annotations)

    val stats =
      (aggregator.all[NonPlacedStatement] collect {
        case NonPlacedStatement(importStat @ Import(_, _), _) =>
          importStat

        case NonPlacedStatement(definition, _)
            if definition.symbol == NoSymbol ||
               definition.symbol.isType ||
               definition.symbol.name == termNames.CONSTRUCTOR  =>
          definition

        case NonPlacedStatement(definition @
            ValDef(mods, name, tpt, rhs), _) =>
          ValDef(
            annotate(mods), name, tpt.typeTree, `#macro`)

        case NonPlacedStatement(definition @
              DefDef(mods, name, tparams, vparamss, tpt, rhs), _) =>
          DefDef(
            annotate(mods), name, tparams, vparamss, tpt.typeTree, `#macro`)
      }) ++
      (aggregator.all[PlacedStatement] collect {
        case PlacedStatement(definition @
              ValDef(mods, name, tpt, rhs), _, _, _, _, _, _)
            if !definition.isRetierSynthetic =>
          ValDef(
            annotate(mods), name, tpt.typeTree, `#macro`)

        case PlacedStatement(definition @
              DefDef(mods, name, tparams, vparamss, tpt, rhs), _, _, _, _, _, _)
            if !definition.isRetierSynthetic =>
          DefDef(
            annotate(mods), name, tparams, vparamss, tpt.typeTree, `#macro`)
      }) ++
      (aggregator.all[PeerDefinition] flatMap { stat =>
        stat.tree +: stat.companion.toList
      })

    val outputStats = stats.zipWithIndex map OutputStatement.tupled

    echo(verbose = true,
      s"  [${outputStats.size} output statements added]")

    aggregator add outputStats
  }
}
