package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait PeerDefinitionCollector { this: Generation =>
  val c: Context
  import c.universe._

  val collectPeerDefinitions = AugmentedAggregation[
    InputStatement, PeerDefinition] {
      aggregator =>

    val peerDefs = aggregator.all[InputStatement] collect {
      _.stat match {
        case peer @ q"""
             $mods class $tpname[..$tparams] $ctorMods(...$paramss)
               extends { ..$earlydefns } with ..$parents { $self =>
               ..$stats
             }""" if peer.symbol.asClass.toType <:< types.peer =>
          (earlydefns, paramss.flatten, parents, stats,
           PeerDefinition(peer, peer.symbol.asClass.toType, parents))

        case peer @ q"""
             $mods trait $tpname[..$tparams]
               extends { ..$earlydefns } with ..$parents { $self =>
               ..$stats
             }""" if peer.symbol.asClass.toType <:< types.peer =>
          (earlydefns, List.empty, parents, stats,
           PeerDefinition(peer, peer.symbol.asClass.toType, parents))
      }
    }

    peerDefs foreach { case (earlydefns, params, parents, stats, _) =>
      if (earlydefns.nonEmpty)
        c.abort(earlydefns(0).pos,
          "early definitions are not allowed for peer types")

      if (params.nonEmpty)
        c.abort(params(0).pos,
          "primary constructors of peer types are not allowed to take arguments")

      stats foreach {
        case DefDef(_, _, _, _, _, _) |
             TypeDef(_, _, _, _) |
             ClassDef(_, _, _, _) =>
        case stat =>
          c.abort(stat.pos,
            "only method and type definitions allowed in peer types")
      }

      parents foreach { parent =>
        parent.symbol.asClass.toType.members foreach { member =>
          if (!member.isType && !member.isMethod)
            c.abort(parent.pos,
              "only method and type definitions allowed in peer type parents")
        }
      }

      // TODO: - no type parameters for peer types to be instantiated
      //       - multiple empty constructor argument lists possible
    }

    val peers = peerDefs map { case (_, _, _, _, decl) => decl }

    echo(
      verbose = true,
      s"Collected peer type definitions " +
      s"(${peers.size} peer type definitions added)")

    aggregator add peers
  }
}
