package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait TransmissionGenerator { this: Generation =>
  val c: Context
  import c.universe._

  val generateTransmissions = UniformAggregation[
    PeerDefinition with PlacedStatement with EnclosingContext] {
      aggregator =>

    echo(verbose = true, s" Generating transmissions for placed expressions")

    val peerSymbols = aggregator.all[PeerDefinition] map { _.peerSymbol }

    val stats = aggregator.all[PlacedStatement] map { stat =>
      stat.copy(expr =
        new TransmissionGenerator(peerSymbols) transform stat.expr)
    }

    echo(verbose = true,
      s"  [${stats.size} placed statements generated, existing replaced]")

    aggregator replace stats
  }

  private class TransmissionGenerator(peerSymbols: List[TypeSymbol])
      extends Transformer {

    def processTransmission(tree: Tree, value: Tree,
        localPeerTypeTag: Option[Tree], remotePeerTypeTag: Tree) = {
      val Seq(_, peerType) = value.tpe.widen.typeArgs

      val messageUnexpectedTree =
        "identifier, selected remote value or remote expression expected"
      val messageUnexpectedMethodTree =
        messageUnexpectedTree +
        " (note that method invocations require a `remote call` expression)"

      val transmissionProperties = value match {
        case _ if types.selection exists { value.tpe <:< _ } =>
          value

        case q"$_.$tname" =>
          val interface = markRetierSynthetic(
            peerInterfaceTree(value, peerType, peerSymbols), value.pos)
          q"$interface.$tname"

        case q"$_.$tname[..$tpts](...$exprss)" =>
          val interface = markRetierSynthetic(
            peerInterfaceTree(value, peerType, peerSymbols), value.pos)
          if (value.isRetierSynthetic)
            q"$interface.$tname[..$tpts](...$exprss)"
          else
            c.abort(value.pos, messageUnexpectedMethodTree)

        case _ =>
          c.abort(value.pos, messageUnexpectedTree)
      }

      val remoteTypeTag = markRetierSynthetic(remotePeerTypeTag, value.pos)

      localPeerTypeTag match {
        case Some(localPeerTypeTag) =>
          val localTypeTag = markRetierSynthetic(localPeerTypeTag, value.pos)

          import trees._
          import names._

          val createTransmission = tree.symbol match {
            case symbols.transmitMultiple => createMultipleTransmission
            case symbols.transmitOptional => createOptionalTransmission
            case symbols.transmitSingle => createSingleTransmission
          }

          q"""$system.$createTransmission($transmissionProperties)(
                $remoteTypeTag, $localTypeTag)"""

        case _ =>
          import trees._
          import names._

          q"""$system.$executeTransmission($transmissionProperties)(
                $remoteTypeTag)"""
      }
    }

    override def transform(tree: Tree) = tree match {
      case q"$_[..$tpts](...$exprss)"
          if symbols.transmit contains tree.symbol =>
        val Seq(_, _, _, _, local, remote, _) = tpts
        val localPeerTypeTag = peerTypeTagTree(
          local.typeTree(abortOnFailure = true), local.tpe, peerSymbols)
        val remotePeerTypeTag = peerTypeTagTree(
          remote.typeTree(abortOnFailure = true), remote.tpe, peerSymbols)

        val ExistentialType(_, TypeRef(pre, sym, _)) = types.peerTypeTag
        val localPeerType = internal typeRef (pre, sym, List(local.tpe))
        val remotePeerType = internal typeRef (pre, sym, List(remote.tpe))

        super.transform(
          processTransmission(
            tree, exprss.head.head,
            Some(internal setType (localPeerTypeTag, localPeerType)),
            internal setType (remotePeerTypeTag, remotePeerType)))

      case q"$expr[..$_](...$exprss)"
          if expr.isRetierSynthetic && expr.symbol == symbols.remoteApply =>
        val Seq(_, remote) = tree.tpe.widen.typeArgs
        val remotePeerTypeTag = peerTypeTagTree(
          TypeTree(remote).typeTree(abortOnFailure = true), remote, peerSymbols)

        val ExistentialType(_, TypeRef(pre, sym, _)) = types.peerTypeTag
        val remotePeerType = internal typeRef (pre, sym, List(remote))

        super.transform(
          processTransmission(
            tree, exprss.head.head, None,
            internal setType (remotePeerTypeTag, remotePeerType)))

      case _ if tree.tpe != null &&
                tree.tpe <:< types.transmissionProvider &&
                (types.bottom forall { tree.tpe <:!< _ }) =>
        c.abort(tree.pos, "unexpected value of type TransmissionProvider")

      case _ =>
        super.transform(tree)
    }
  }
}
