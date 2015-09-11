package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait TransmissionGenerator { this: Generation =>
  val c: Context
  import c.universe._
  import trees._
  import names._

  val generateTransmissions = UniformAggregation[
    PlacedStatement with PeerDefinition with EnclosingContext] {
      aggregator =>

    echo(verbose = true, s" Generating transmissions for placed expressions")

    val enclosingName = aggregator.all[EnclosingContext].head.name

    val peerTypes = aggregator.all[PeerDefinition] map { _.peerType }

    val stats = aggregator.all[PlacedStatement] map { stat =>
      stat.copy(expr =
        new TransmissionGenerator(stat, enclosingName, peerTypes)
          transform stat.expr)
    }

    echo(verbose = true,
      s"  [${stats.size} placed statements generated, existing replaced]")

    aggregator replace stats
  }

  private class TransmissionGenerator(stat: PlacedStatement,
    enclosingName: TypeName, peerTypes: List[Type])
      extends Transformer {
    override def transform(tree: Tree) = tree match {
      case tree @ q"$_[..$_](...$exprss)"
          if symbols.transmit contains tree.symbol =>
        val Seq(Seq(value), Seq(_, transmissionProvider)) = exprss

        val Seq(_, (remoteType, remoteName), (_, localName)) =
          transmissionProvider.tpe.typeArgs.head.typeArgs map { tpe =>
            (tpe, tpe.typeSymbol.name.toTermName)
          }

        if (!(peerTypes contains remoteType)) {
          val companionSymbol = remoteType.typeSymbol.companion
          val companionType =
            if (companionSymbol.isModule)
              companionSymbol.asModule.moduleClass.asType.toType
            else
              NoType

          if ((companionType member names.interface) == NoSymbol)
            c.abort(value.pos,
              "cannot access peer type interface " +
              "(maybe peer definition was not placed " +
              "inside `multitier` environment)")
        }

        val localCompanion = q"$enclosingName.this.$localName"

        val q"$expr.$_[..$_](...$_)" = value
        val remoteCompanion = q"$expr.$remoteName"

        val remote = q"$remoteCompanion.$peerTypeTag"

        val local = q"$localCompanion.$peerTypeTag"

        val transmissionProperties = value match {
          case q"$_.$tname[..$tpts](...$exprss)" =>
            q"$remoteCompanion.$interface.$tname[..$tpts](...$exprss)"
          case _ =>
            c.abort(value.pos,
              "identifier, selected remote value or remote expression expected")
        }

        val createTransmission = TermName(tree.symbol match {
          case symbols.transmitMultiple => "createMultipleTransmission"
          case symbols.transmitOptional => "createOptionalTransmission"
          case symbols.transmitSingle => "createSingleTransmission"
        })

        markRetierSynthetic(
          q"""$system.$createTransmission($transmissionProperties)(
              $remote, $local)""",
          value.pos)

      case _ if tree.tpe <:< types.transmissionProvider &&
                (types.bottom forall { tree.tpe <:!< _ }) =>
        c.abort(tree.pos, "unexpected value of type TransmissionProvider")

      case _ =>
        super.transform(tree)
    }
  }
}
