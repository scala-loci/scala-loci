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

        val module = code.tree.symbol.info

        val peers = new Peers(module.decls.toSet)

        val peerDecls =
          (code.body collect (scala.Function unlift {
            case tree @ q"$_ type $_[..$_] = $tpt" =>
              peers.checkPeerType(tree.symbol, tpt, tree.pos)
            case _ =>
              None
          })) ++
          (module.members collect (scala.Function unlift { symbol =>
            if (symbol.isType && (module decl symbol.name) == NoSymbol)
              peers.checkPeerType(symbol, symbol.pos)
            else
              None
          }))

        case class PeerImpl(peer: Peer, bases: List[Tree], body: List[Tree])

        val peerImpls =
          peerDecls map { case peer @ Peer(symbol, name, _, bases, _) =>
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

            // collect super peer types
            // i.e., peer types in the explicitly specified upper bound
            val (inheritedSuperPeers, delegatedSuperPeers) = (bases
              collect { case (base, baseTree) if !baseTree.isEmpty =>
                peers.requirePeerType(base, symbol.pos) -> baseTree
              }
              partition { case (base, _) =>
                (module member base.name) != NoSymbol
              })

            // inherit implementation for peer bases defined in the same module
            val inheritedBases =
              inheritedSuperPeers map { case (peer, _) => tq"${peer.name}" }

            // delegate implementation for peer bases defined in a different module
            val delegatedBases =
              delegatedSuperPeers map { case (peer, baseTree) =>
                val peerImpl = baseTree match {
                  case tq"$_#$_" => Left("type projection")
                  case tq"$_.type" => Left("singleton type")
                  case tq"$_ forSome { ..$_ }" => Left("existential type")
                  case tq"$ref.$_[..$_]" => Right(tq"$ref.${peer.name}")
                  case tq"$tpname" => Right(tq"${peer.name}")
                }

                val tree = peerImpl match {
                  case Left(desc) =>
                    c.abort(baseTree.pos,
                      s"peer type cannot be a subtype of $desc $baseTree")
                  case Right(tree) =>
                    tree
                }

                q"val ${peer.uniqueName}: $tree"
              }

            PeerImpl(peer, overriddenBases ++ inheritedBases, delegatedBases)
          }

        val result = code replaceBody (code.body ++ (peerImpls map { peerImpl =>
          q"trait ${peerImpl.peer.name} extends ..${peerImpl.bases} { ..${peerImpl.body} }"
        }))

        result.untypechecked.tree
    }

    companion.headOption map { companion => q"$result; $companion"} getOrElse result
  }
}
