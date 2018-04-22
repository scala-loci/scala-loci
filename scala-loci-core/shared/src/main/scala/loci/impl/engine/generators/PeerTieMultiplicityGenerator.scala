package loci
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait PeerTieMultiplicityGenerator { this: Generation =>
  val c: Context
  import c.universe._

  val generatePeerTieMultiplicities = AugmentedAggregation[
    PeerDefinition with PlacedStatement, PeerTieMultiplicity] {
      aggregator =>

    echo(verbose = true, " Generating peer tie multiplicities")

    val tieTypeName = names.Tie.toTypeName

    val multiplicities = aggregator.all[PeerDefinition] flatMap { definition =>
      val ClassDef(_, _, _, Template(_, _, body)) = definition.tree

      val expressions = aggregator.all[PlacedStatement] collect {
        case stat if stat.peerSymbol == definition.peerSymbol => stat.expr
      }

      val multiplicities = body collectFirst {
        case typeDef @ TypeDef(_, `tieTypeName`, List(), rhs) =>
          val single = 1
          val optional = 2
          val multiple = 3
          def string(multiplicity: Int) = multiplicity match {
            case `single` => "single"
            case `optional` => "optional"
            case `multiple` => "multiple"
          }

          val tie = rhs.typeTree match {
            case TypeBoundsTree(lo, _) if lo.tpe =:!= definitions.NothingTpe =>
              c.abort(lo.pos,
                s"lower type bounds not allowed for $tieTypeName type")
            case TypeBoundsTree(_, hi) => hi
            case _ => rhs
          }

          val ties = tie.typeTree match {
            case CompoundTypeTree(Template(parents, noSelfType, List())) =>
              val RefinedType(parentTypes, _) = tie.tpe
              parents zip parentTypes map { case (tree, tpe) =>
                internal setType (tree, tpe)
              }
            case CompoundTypeTree(Template(_, noSelfType, body)) =>
              c.abort(body.head.pos,
                s"definitions not allowed for $tieTypeName type")
            case CompoundTypeTree(Template(_, self, _)) =>
              c.abort(self.pos,
                s"self-type not allowed for $tieTypeName type")
            case tree =>
              List(internal setType (tree, tie.tpe))
          }

          val multiplicities = ties map { tie =>
            if (tie.tpe <:< types.single)
              (single, tie)
            else if (tie.tpe <:< types.optional)
              (optional, tie)
            else if (tie.tpe <:< types.multiple)
              (multiple, tie)
            else
              c.abort(tie.pos,
                s"only multiple, optional or single compounds " +
                s"allowed for $tieTypeName type")
          } map {
            case (multiplicity, tie) =>
              val AppliedTypeTree(_, List(arg)) = tie
              val argType = tie.tpe.underlying.typeArgs.head

              if (argType <:!< types.peer || argType =:= definitions.NothingTpe)
                c.abort(arg.pos, "peer type expected")

              (tie, internal setType (arg, argType), multiplicity)
          }


          val combinations = multiplicities combinations 2

          combinations foreach { case Seq((_, peer0, _), (_, peer1, _)) =>
            if (peer0.tpe =:!= peer1.tpe &&
                peer0.tpe.typeSymbol == peer1.tpe.typeSymbol)
              c.abort(typeDef.pos,
                s"using distinct types referring to the same peer type " +
                s"is not allowed: $peer0 and $peer1")
          }

          combinations foreach { case Seq((_, peer0, _), (_, peer1, _)) =>
            if (peer0.tpe =:!= peer1.tpe &&
                peer0.tpe.typeSymbol.name == peer1.tpe.typeSymbol.name)
              c.abort(typeDef.pos,
                s"using distinct peer types of the same name " +
                s"is not allowed: $peer0 and $peer1")
          }

          combinations foreach {
            case Seq((tie0, peer0, _), (tie1, peer1, _)) =>
              if (peer0.tpe =:= peer1.tpe)
                c.abort(typeDef.pos,
                  s"multiple occurrences of same peer type " +
                  s"are not allowed: $tie0 and $tie1")
          }


          val statedPeerMultiplicities =
            (multiplicities map { case (_, peer, multiplicity) =>
              val typeSymbol = peer.tpe.typeSymbol
              val symbol = typeSymbol.typeSignature match {
                case TypeBounds(_, hi) if typeSymbol.isParameter =>
                  hi.typeSymbol
                case _ =>
                  typeSymbol
              }
              symbol -> multiplicity
            }).toMap

          val peerBaseMultiplicities = statedPeerMultiplicities map {
            case (peer, multiplicity) =>
              val bases = peer.asType.toType.baseClasses filter {
                _.asType.toType <:< types.peer
              }
              (peer, bases, multiplicity)
          }

          val peerBases = (peerBaseMultiplicities flatMap {
            case (_, bases, _) => bases
          }).toSet

          peerBases groupBy { _.name } foreach { case (name, peers) =>
            if (peers.size > 1)
              c.abort(typeDef.pos,
                s"distinct peer type parents of the same name " +
                s"are not allowed: $name")
          }

          val combinedPeerBaseMultiplicities =
            peerBaseMultiplicities flatMap { case (_, bases, multiplicity) =>
              bases map { (_, multiplicity) }
            } groupBy { case (peer, _) =>
              peer
            } map { case (peer, multiplicities) =>
              peer -> (multiplicities map {
                case (peer, multiplicity) => multiplicity
              }).max
            }

          val leafPeerBases =
            peerBaseMultiplicities filterNot { case (peer, _, _) =>
              peerBaseMultiplicities exists { case (other, bases, _) =>
                (peer != other) && (bases contains peer)
              }
            } flatMap { case (_, bases, _) =>
              bases
            }

          val potentialPeerMultiplicities =
            combinedPeerBaseMultiplicities map { case (peer, multiplicity) =>
              val count = leafPeerBases count { _ == peer }
              peer -> (if (count > 1) multiple else multiplicity)
            }

          potentialPeerMultiplicities foreach { case (peer, multiplicity) =>
            (multiplicity, statedPeerMultiplicities get peer) match {
              case (potential @ (`optional` | `single`), None) =>
                val peerAccessed = expressions exists {
                  _ exists { tree =>
                    tree.tpe != null && (tree.tpe contains peer)
                  }
                }
                if (peerAccessed)
                  c.warning(typeDef.pos,
                    s"tie for ${peer.name} not specified " +
                    s"(inferred as ${string(multiple)}, " +
                    s"but can be specified as ${string(potential)})")

              case (potential, Some(stated)) if potential > stated =>
                c.abort(typeDef.pos,
                  s"tie for ${peer.name} specified as " +
                  s"${string(stated)}, but must be ${string(potential)}")

              case _ =>
            }
          }

          multiplicities map { case (_, peer, multiplicity) =>
            PeerTieMultiplicity(definition.peerSymbol, peer,
              multiplicity match {
                case `single` => trees.SingleTie
                case `optional` => trees.OptionalTie
                case `multiple` => trees.MultipleTie
              })
          }
      }

      multiplicities getOrElse List.empty
    }

    echo(verbose = true,
      s"  [${multiplicities.size} peer tie multiplicities added]")

    aggregator add multiplicities
  }
}
