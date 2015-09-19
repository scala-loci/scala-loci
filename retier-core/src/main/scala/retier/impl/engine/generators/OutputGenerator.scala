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
      (aggregator.all[NonPlacedStatement] map { stat =>
        OutputStatement(stat.tree)
      }) ++
      (aggregator.all[PlacedStatement] collect {
        case PlacedStatement(
            ValDef(mods, name, tpt, rhs), _, _, _, _, _) =>
          OutputStatement(ValDef(annotate(mods), name,
            tpt.typeTree, `#macro`))

        case PlacedStatement(
            DefDef(mods, name, tparams, vparamss, tpt, rhs), _, _, _, _, _) =>
          OutputStatement(DefDef(annotate(mods), name, tparams, vparamss,
            tpt.typeTree, `#macro`))
      }) ++
      (aggregator.all[PeerDefinition] flatMap { stat =>
        OutputStatement(stat.tree) +:
          (stat.companion map OutputStatement).toList
      })

    echo(verbose = true,
      s"  [${stats.size} output statements added]")

    aggregator add stats
  }
}
