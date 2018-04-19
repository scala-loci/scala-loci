package loci
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

    echo(verbose = true, " Collecting peer type definitions")

    def cleanParents(parents: List[Tree]) =
      (parents map {
        case parent: TypeTree =>
          if (parent.original != null) {
            if (parent.original.tpe != null)
              Some(parent.original)
            else
              Some(internal setType (parent.original, parent.tpe))
          }
          else
            None
        case parent => Some(parent)
      }).flatten

    val peerDefs = aggregator.all[InputStatement] collect {
      case InputStatement(peer @ q"""
           $mods class $tpname[..$tparams] $ctorMods(...$paramss)
             extends { ..$earlydefns } with ..$parents { $self =>
             ..$stats
           }""", index) if peer.symbol.asClass.toType <:< types.peer =>
        val sym = peer.symbol.asClass
        (earlydefns, paramss, parents, stats,
         PeerDefinition(
          peer, sym, tparams, paramss, cleanParents(parents),
          typer cleanModifiers mods, stats, isClass = true, None, index))

      case InputStatement(peer @ q"""
           $mods trait $tpname[..$tparams]
             extends { ..$earlydefns } with ..$parents { $self =>
             ..$stats
           }""", index) if peer.symbol.asClass.toType <:< types.peer =>
        val sym = peer.symbol.asClass
        (earlydefns, List.empty, parents, stats,
         PeerDefinition(
          peer, sym, tparams, List.empty, cleanParents(parents),
          typer cleanModifiers mods, stats, isClass = false, None, index))
    }

    peerDefs foreach { case (earlydefns, params, parents, stats, _) =>
      if (earlydefns.nonEmpty)
        c.abort(earlydefns.head.pos,
          "early definitions are not allowed for peer types")

      if (params.nonEmpty && params.head.nonEmpty &&
          !(params.head.head.mods hasFlag Flag.IMPLICIT))
        c.abort(params.head.head.pos,
          "primary constructors of peer types are not allowed " +
          "to take explicit arguments")
    }

    val decls = peerDefs map { case (_, _, _, _, decl) => decl }

    val names = (decls.zipWithIndex map { case (decl, index) =>
      decl.peerSymbol.name.toTermName -> index
    }).toMap

    val peers = aggregator.all[InputStatement].foldLeft(decls) {
      case (decls, InputStatement(companion @ q"""
          $mods object $tname
            extends { ..$earlydefns } with ..$parents { $self =>
            ..$body
          }""", _)) if names contains tname =>
        val index = names(tname)
        val decl = decls(index)
        decls updated (index, decl copy (companion = Some(companion)))

      case (decls, _) =>
        decls
    }

    echo(verbose = true, s"  [${peers.size} peer type definitions added]")

    aggregator add peers
  }
}
