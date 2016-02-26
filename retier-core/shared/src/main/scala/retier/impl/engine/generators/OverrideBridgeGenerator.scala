package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait OverrideBridgeGenerator { this: Generation =>
  val c: Context
  import c.universe._

  val generateOverrideBridge = UniformAggregation[PlacedStatement] {
      aggregator =>

    echo(verbose = true, " Generating override bridge methods")

    def validatePlacedResultTypes(base: Type, sub: Type, pos: Position) =
      if (sub.underlying.typeArgs.head <:!< base.underlying.typeArgs.head)
        c.abort(pos, "overriding abstraction has incompatible type")

    val stats = aggregator.all[PlacedStatement] collect {
      case stat @ PlacedStatement(
          definition @ ValDef(mods, name, tpt, _), _, _, _,
          Some(overridingDecl), _, _) =>
        validatePlacedResultTypes(
          overridingDecl.typeSignature.finalResultType, tpt.tpe, definition.pos)

        val expr = q"$name"
        val overridingDefinition = ValDef(
          Modifiers(
            mods.flags | Flag.OVERRIDE | Flag.SYNTHETIC | Flag.ARTIFACT,
            mods.privateWithin,
            mods.annotations),
          overridingDecl.name, tpt, expr)

        stat.copy(
          tree = markRetierSynthetic(overridingDefinition, definition.pos),
          overridingDecl = None,
          expr = expr)

      case stat @ PlacedStatement(
          definition @ DefDef(mods, name, tparams, vparamss, tpt, _), _, _, _,
          Some(overridingDecl), _, _) =>
        validatePlacedResultTypes(
          overridingDecl.typeSignature.finalResultType, tpt.tpe, definition.pos)
        if (overridingDecl.isStable)
          c.abort(definition.pos, "overriding abstraction needs to be stable")

        val expr = q"$name(...${vparamss map { _ map { _.name } } })"
        val overridingDefinition = DefDef(
          Modifiers(
            mods.flags | Flag.OVERRIDE | Flag.SYNTHETIC | Flag.ARTIFACT,
            mods.privateWithin,
            mods.annotations),
          overridingDecl.name, tparams, vparamss, tpt, expr)

        stat.copy(
          tree = markRetierSynthetic(overridingDefinition, definition.pos),
          overridingDecl = None,
          expr = expr)
    }

    echo(verbose = true, s"  [${stats.size} placed statements added]")

    aggregator add stats
  }
}
