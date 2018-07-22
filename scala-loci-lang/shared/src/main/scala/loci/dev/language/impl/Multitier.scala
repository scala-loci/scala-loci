package loci.dev
package language
package impl

import scala.reflect.macros.blackbox.Context

class Multitier(val c: Context) extends MultitierCode with Peers {
  import c.universe._

  def annotation(annottees: Tree*): Tree = {
    val annottee :: companion = annottees

    // the current macro expansion always appears twice
    // see: http://stackoverflow.com/a/20466423
    val recursionCount = c.openMacros.count { check =>
      c.enclosingPosition == check.enclosingPosition &&
      c.macroApplication.toString == check.macroApplication.toString
    }
    val isRecursiveExpansion = recursionCount > 2

    val code = annottee match {
      case ClassDef(_, _, _, _) | ModuleDef(_, _, _)
          if c.hasErrors || isRecursiveExpansion =>
        Left(annottee)
      case ClassDef(_, _, _, Template(_, _, body)) =>
        Right(new MultitierClass(annottee))
      case ModuleDef(_, _, Template(_, _, body)) =>
        Right(new MultitierModule(annottee))
      case _ =>
        c.abort(
          c.enclosingPosition,
          "multitier annotation only applicable to classes, traits or objects")
    }

    val result = code match {
      case Left(annottee) =>
        annottee

      case Right(untypedCode) =>
        val code = untypedCode.typechecked

        val peers = new Peers(
          (code.tree collect { case tree: DefTree => tree.symbol }).toSet)

        val peerDecls =
          code.tree.symbol.info.members collect (scala.Function unlift { symbol =>
            peers.checkPeerType(symbol, symbol.pos)
          })

        case class PeerImpl(peer: Peer, bases: List[Tree])

        val peerImpls =
          peerDecls map { case peer @ Peer(symbol, name, bases, _) =>
            // collect overridden peer types
            // i.e., types of the same name in the module base types
            val overriddenBases =
              code.bases collect (scala.Function unlift { base =>
                val basePeer = base.tpe member symbol.name
                if (basePeer != NoSymbol) {
                  peers.requirePeerType(basePeer, symbol.pos)
                  Some(tq"super[${base.symbol.name.toTypeName}].$name")
                }
                else
                  None
              })

            // collect inherited peer types
            // i.e., explicitly specified super peer types
            val inheritedBases =
              bases map { base =>
                val peer = peers.requirePeerType(base, symbol.pos)
                tq"${peer.name}"
              }

            PeerImpl(peer, overriddenBases ++ inheritedBases)
          }

        val result = code replaceBody (code.body ++ (peerImpls map { peerImpl =>
          q"trait ${peerImpl.peer.name} extends ..${peerImpl.bases}"
        }))

        result.untypechecked.tree
    }

    companion.headOption map { companion => q"$result; $companion"} getOrElse result
  }
}
