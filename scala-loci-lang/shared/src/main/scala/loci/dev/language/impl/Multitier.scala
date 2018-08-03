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
      case ClassDef(_, _, _, Template(_, _, _)) =>
        Right(new MultitierClass(annottee))
      case ModuleDef(_, _, Template(_, _, _)) =>
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

        val moduleSymbol = code.tree.symbol
        val moduleType = moduleSymbol.info

        val peers = new Peers(moduleType.decls.toSet)

        val peerDecls =
          (code.body collect (scala.Function unlift {
            case tree @ q"$_ type $_[..$_] = $tpt" =>
              peers.checkPeerType(tree.symbol, tpt, tree.pos)
            case _ =>
              None
          })) ++
          (moduleType.members collect (scala.Function unlift { symbol =>
            if (symbol.isType && (moduleType decl symbol.name) == NoSymbol)
              peers.checkPeerType(symbol, symbol.pos)
            else
              None
          }))

        case class PeerImpl(peer: Peer, bases: List[Tree], body: List[Tree])

        val peerImpls =
          peerDecls map { case peer @ Peer(symbol, name, bases, _) =>
            // inherit implementation for overridden peer types
            // i.e., types of the same name in the module base types
            val overriddenBases =
              code.bases collect (scala.Function unlift { base =>
                val basePeer = base.tpe member symbol.name
                if (basePeer != NoSymbol)
                  Some(tq"super[${base.symbol.name.toTypeName}].$name")
                else
                  None
              })

            // inherit implementation for peer bases defined in the same module
            val inheritedBases = bases collect {
              case Peer.InheritedBase(_, name, tree)
                  if !tree.isEmpty =>
                tq"$name"
            }

            // delegate implementation for peer bases defined in a different module
            val delegatedBases = bases collect {
              case Peer.DelegatedBase(tpe, _, name, tree)
                  if !tree.isEmpty && (moduleType member name) == NoSymbol =>
                val peer = peers.requirePeerType(tpe.typeSymbol)

                tree match {
                  case tq"$ref.$_[..$_]" =>
                    q"final val $name: $ref.${peer.name} = $ref.${peer.name.toTermName}"
                  case tq"$tpname" =>
                    q"final val $name: ${peer.name} = ${peer.name.toTermName}"
                }
            }

            PeerImpl(peer, overriddenBases ++ inheritedBases, delegatedBases)
          }

        val result = code replaceBody (code.body ++ (peerImpls flatMap { peerImpl =>
          Seq(
            q"${Flag.SYNTHETIC} trait ${peerImpl.peer.name} extends ..${peerImpl.bases} { ..${peerImpl.body} }",
            if (moduleSymbol.isAbstract)
              q"${Flag.SYNTHETIC} def ${peerImpl.peer.name.toTermName}: ${peerImpl.peer.name}"
            else
              q"${Flag.SYNTHETIC} def ${peerImpl.peer.name.toTermName}: ${peerImpl.peer.name} = new ${peerImpl.peer.name} { }")
        }))

        result.untypechecked.tree
    }

    companion.headOption map { companion => q"$result; $companion"} getOrElse result
  }
}
