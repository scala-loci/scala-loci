package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait OutputGenerator { this: Generation =>
  val c: Context
  import c.universe._
  import trees._

  object implicitPeerTypeTagProcessor extends Transformer {
    override def transform(tree: Tree) = tree match {
      case q"$expr[..$tpts](...$exprss)"
          if tree.symbol != null && tree.symbol.isMethod =>

        val params = tree.symbol.asMethod.paramLists.lastOption.toList.flatten
        val implicitPeerTypeTag = params exists { param =>
          param.isImplicit && param.typeSignature <:< types.peerTypeTag
        }

        if (implicitPeerTypeTag) {
          val processedExpr = super.transform(expr)
          val processedTpts = tpts map transform
          val processedExprss = exprss dropRight 1 map { _ map transform }
          q"$processedExpr[..$processedTpts](...$processedExprss)"
        }
        else
          super.transform(tree)

      case _ =>
        super.transform(tree)
    }
  }

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

    def nullValue(typeTree: Tree) =
      q"null.asInstanceOf[${typeTree.typeTree}]"

    val stats =
      (aggregator.all[NonPlacedStatement] collect {
        case NonPlacedStatement(importStat @ Import(_, _), _) =>
          importStat

        case stat @ NonPlacedStatement(tree, _) if !stat.isPeerBound =>
          tree

        case NonPlacedStatement(definition @
            ValDef(mods, name, tpt, rhs), _) =>
          ValDef(
            annotate(mods), name, tpt.typeTree, nullValue(tpt))

        case NonPlacedStatement(definition @
              DefDef(mods, name, tparams, vparamss, tpt, rhs), _) =>
          DefDef(
            annotate(mods), name, tparams, vparamss, tpt.typeTree, nullValue(tpt))
      }) ++
      (aggregator.all[PlacedStatement] collect {
        case PlacedStatement(definition @
              ValDef(mods, name, tpt, rhs), _, _, _, _, _, _)
            if !definition.isRetierSynthetic =>
          ValDef(
            annotate(mods), name, tpt.typeTree, nullValue(tpt))

        case PlacedStatement(definition @
              DefDef(mods, name, tparams, vparamss, tpt, rhs), _, _, _, _, _, _)
            if !definition.isRetierSynthetic =>
          DefDef(
            annotate(mods), name, tparams, vparamss, tpt.typeTree, nullValue(tpt))
      }) ++
      (aggregator.all[PeerDefinition] flatMap { stat =>
        stat.tree +: stat.companion.toList
      }) map
      implicitPeerTypeTagProcessor.transform

    val outputStats = stats.zipWithIndex map OutputStatement.tupled

    echo(verbose = true,
      s"  [${outputStats.size} output statements added]")

    aggregator add outputStats
  }
}
