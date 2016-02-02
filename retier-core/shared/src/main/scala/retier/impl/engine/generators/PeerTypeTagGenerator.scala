package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait PeerTypeTagGenerator { this: Generation =>
  val c: Context
  import c.universe._

  val generatePeerTypeTags = UniformAggregation[PeerDefinition] {
      aggregator =>

    echo(verbose = true, " Generating peer type tags")

    val synthetic = Flag.SYNTHETIC
    val peerSymbols = aggregator.all[PeerDefinition] map { _.peerSymbol }

    def wildcardedTypeTree(expr: Tree, typeArgsCount: Int) =
      if (typeArgsCount == 0)
        expr
      else {
        val wildcards = ((0 until typeArgsCount) map { _ =>
          TypeName(c freshName "_")
        }).toList

        ExistentialTypeTree(
          AppliedTypeTree(expr, wildcards map { Ident(_) }),
          wildcards map { TypeDef(
            Modifiers(Flag.DEFERRED | Flag.SYNTHETIC), _, List.empty,
            TypeBoundsTree(EmptyTree, EmptyTree))
          })
      }

    def createImplicitPeerTypeTag(peerDefinition: PeerDefinition) = {
      import trees._
      import names._

      val PeerDefinition(_, peerSymbol, typeArgs, _, parents, _, _, _, _, _) =
        peerDefinition

      val peerName = peerSymbol.name
      val name = peerName.toString
      val wildcardedPeerType = wildcardedTypeTree(tq"$peerName", typeArgs.size)

      val bases = parents collect {
        case parent if parent.tpe =:= types.peer =>
          q"$Peer.$peerTypeTag.$peerType"

        case parent if parent.tpe <:< types.peer =>
          val peerTypeTag = peerTypeTagTree(parent, parent.tpe, peerSymbols)
          q"$peerTypeTag.$peerType"
      }

      q"""$synthetic implicit val $peerTypeTag
        : $PeerTypeTag[$wildcardedPeerType] =
        $PeerTypeTagCreate[$wildcardedPeerType]($name, $List(..$bases))"""
    }

    def processPeerCompanion(peerDefinition: PeerDefinition) = {
      import names._

      val PeerDefinition(_, peerSymbol, _, _, _, _, _, _, companion, _) =
        peerDefinition

      val companionName = peerSymbol.name.toTermName
      val implicitPeerTypeTag = createImplicitPeerTypeTag(peerDefinition)

      val generatedCompanion = companion match {
        case Some(q"""$mods object $tname
                    extends { ..$earlydefns } with ..$parents { $self =>
                    ..$stats
                  }""") =>
          stats foreach {
            case stat: DefTree if stat.name == peerTypeTag =>
              c.abort(stat.pos,
                s"member of name `$peerTypeTag` not allowed " +
                s"in peer type companion objects")
            case _ =>
          }

          parents foreach { parent =>
            if ((parent.tpe member peerTypeTag) != NoSymbol)
              c.abort(parent.pos,
                s"member of name `$peerTypeTag` not allowed " +
                s"in peer type companion object parents")
          }

          q"""$mods object $tname
            extends { ..$earlydefns } with ..$parents { $self =>
            ${markRetierSynthetic(implicitPeerTypeTag)}
            ..$stats
          }"""

        case _ =>
          markRetierSynthetic(
            q"$synthetic object $companionName { $implicitPeerTypeTag }")
      }

      peerDefinition.copy(companion = Some(generatedCompanion))
    }

    val definitions = aggregator.all[PeerDefinition] map processPeerCompanion

    echo(verbose = true,
      s"  [${definitions.size} peer definitions generated, existing replaced]")

    aggregator replace definitions
  }
}
