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
    PeerDefinition with PlacedStatement with EnclosingContext] {
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
      case tree @ q"$_[..$tpts](...$exprss)"
          if symbols.transmit contains tree.symbol =>
        val Seq(_, _, local, remote, _) = tpts
        val Seq(Seq(value), Seq(_, transmissionProvider)) = exprss
        val Seq(_, peerType) = value.tpe.widen.typeArgs

        val localPeerTypeTag = peerTypeTagTree(
          local.typeTree(abortOnFailure = true), local.tpe, peerTypes)

        val remotePeerTypeTag = peerTypeTagTree(
          remote.typeTree(abortOnFailure = true), remote.tpe, peerTypes)

        val messageUnexpectedTree =
          "identifier, selected remote value or remote expression expected"
        val messageUnexpectedMethodTree =
          messageUnexpectedTree +
          " (note that method invocations require a `remote call` expression)"

        val transmissionProperties = value match {
          case _ if types.selection exists { value.tpe <:< _ } =>
            value

          case q"$_.$tname" =>
            val interface = peerInterfaceTree(value, peerType, peerTypes)
            q"$interface.$tname"

          case q"$_.$tname[..$tpts](...$exprss)" =>
            val interface = peerInterfaceTree(value, peerType, peerTypes)
            if (value.isRetierSynthetic)
              q"$interface.$tname[..$tpts](...$exprss)"
            else
              c.abort(value.pos, messageUnexpectedMethodTree)

          case _ =>
            c.abort(value.pos, messageUnexpectedTree)
        }

        val createTransmission = tree.symbol match {
          case symbols.transmitMultiple => createMultipleTransmission
          case symbols.transmitOptional => createOptionalTransmission
          case symbols.transmitSingle => createSingleTransmission
        }

        val localTypeTag = markRetierSynthetic(localPeerTypeTag, value.pos)
        val remoteTypeTag = markRetierSynthetic(remotePeerTypeTag, value.pos)
        val props = markRetierSynthetic(transmissionProperties, value.pos)

        super.transform(
          q"$system.$createTransmission($props)($remoteTypeTag, $localTypeTag)")

      case _ if tree.tpe != null &&
                tree.tpe <:< types.transmissionProvider &&
                (types.bottom forall { tree.tpe <:!< _ }) =>
        c.abort(tree.pos, "unexpected value of type TransmissionProvider")

      case _ =>
        super.transform(tree)
    }
  }
}
