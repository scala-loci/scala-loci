package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait NamesValidator { this: Generation =>
  val c: Context
  import c.universe._

  val validateNames = UniformAggregation[
    EnclosingContext with InputStatement with PeerDefinition] {
      aggregator =>

    echo(verbose = true, " Validating identifier names")

    def validatePeerTypeNames() = {
      val inheritedPeerTypes =
        (aggregator.all[EnclosingContext].head.bases flatMap { base =>
          base.tpe.members collect {
            case member
              if member.isClass && member.asType.toType <:< types.peer => member
          } map { _.asType.name -> base}
        } groupBy { case (name, _) => name } map { case (name, peerBases) =>
          val (_, bases) = peerBases.unzip
          name -> bases
        }).toMap

      inheritedPeerTypes foreach { case (name, bases) =>
        if (bases.size > 1) {
          val msg = s"more than one peer definition for $name in parents"
          bases dropRight 1 foreach { base => c.error(base.pos, msg) }
          c.abort(bases.last.pos, msg)
        }
      }

      aggregator.all[PeerDefinition] foreach { peer =>
        (inheritedPeerTypes get peer.peerName) match {
          case (Some(bases)) =>
            val msg = s"peer ${peer.peerName} already defined in parent"
            c.error(peer.tree.pos, msg)
            bases dropRight 1 foreach { base => c.error(base.pos, msg) }
            c.abort(bases.last.pos, msg)
          case _ =>
        }
      }
    }

    def validateNames() = {
      aggregator.all[InputStatement] foreach {
        _.stat foreach {
          case tree: DefTree if isRetierName(tree.name) =>
            c.abort(tree.pos,
              "identifier name not allowed in `multitier` environment")
          case _ =>
        }
      }

      aggregator.all[EnclosingContext].head.bases foreach { base =>
        base.tpe.members foreach { member =>
          if (isRetierName(member.name))
            c.abort(base.pos,
              "identifier name not allowed in `multitier` environment: " +
              member.name)
        }
      }
    }

    validatePeerTypeNames

    echo(verbose = true, "  [peer names validated]")

    validateNames

    echo(verbose = true, "  [identifier names validated]")

    aggregator
  }
}
