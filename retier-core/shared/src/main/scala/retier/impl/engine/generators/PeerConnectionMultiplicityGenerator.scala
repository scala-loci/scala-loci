package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait PeerConnectionMultiplicityGenerator { this: Generation =>
  val c: Context
  import c.universe._

  val generatePeerConnectionMultiplicities = AugmentedAggregation[
    PeerDefinition with PlacedStatement, PeerConnectionMultiplicity] {
      aggregator =>

    echo(verbose = true, " Generating peer Connection multiplicities")

    val connectionTypeName = names.connection.toTypeName

    val multiplicities = aggregator.all[PeerDefinition] flatMap { definition =>
      val ClassDef(_, _, _, Template(_, _, body)) = definition.tree

      val expressions = aggregator.all[PlacedStatement] collect {
        case stat if stat.peerSymbol == definition.peerSymbol => stat.expr
      }

      val multiplicities = body collectFirst {
        case typeDef @ TypeDef(_, connectionTypeName, List(), rhs) =>
          val single = 1
          val optional = 2
          val multiple = 3
          def string(multiplicity: Int) = multiplicity match {
            case `single` => "single"
            case `optional` => "optional"
            case `multiple` => "multiple"
          }

          val connection = rhs.typeTree match {
            case TypeBoundsTree(lo, _) if lo.tpe =:!= definitions.NothingTpe =>
              c.abort(lo.pos,
                "lower type bounds not allowed for connection type")
            case TypeBoundsTree(_, hi) => hi
            case _ => rhs
          }

          val connections = connection.typeTree match {
            case CompoundTypeTree(Template(parents, noSelfType, List())) =>
              val RefinedType(parentTypes, _) = connection.tpe
              parents zip parentTypes map { case (tree, tpe) =>
                internal setType (tree, tpe)
              }
            case CompoundTypeTree(Template(_, noSelfType, body)) =>
              c.abort(body.head.pos,
                "definitions not allowed for connection type")
            case CompoundTypeTree(Template(_, self, _)) =>
              c.abort(self.pos,
                "self-type not allowed for connection type")
            case tree =>
              List(internal setType (tree, connection.tpe))
          }

          val multiplicities = connections map { connection =>
            if (connection.tpe <:< types.single)
              (single, connection)
            else if (connection.tpe <:< types.optional)
              (optional, connection)
            else if (connection.tpe <:< types.multiple)
              (multiple, connection)
            else
              c.abort(connection.pos,
                "only multiple, optional or single compounds " +
                "allowed for connection type")
          } map {
            case (multiplicity, connection) =>
              val AppliedTypeTree(_, List(arg)) = connection
              val argType = connection.tpe.typeArgs.head

              if (argType <:!< types.peer || argType =:= definitions.NothingTpe)
                c.abort(arg.pos, "peer type expected")

              (connection, internal setType (arg, argType), multiplicity)
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
            case Seq((connection0, peer0, _), (connection1, peer1, _)) =>
              if (peer0.tpe =:= peer1.tpe)
                c.abort(typeDef.pos,
                  s"multiple occurrences of same peer type " +
                  s"are not allowed: $connection0 and $connection1")
          }


          val statedPeerMultiplicities =
            (multiplicities map { case (_, peer, multiplicity) =>
              val typeSymbol = peer.tpe.typeSymbol
              val symbol = typeSymbol.typeSignature match {
                case TypeBounds(_, hi)
                  if typeSymbol.isParameter => hi.typeSymbol
                case _ => typeSymbol
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
                    s"connection for ${peer.name} not specified " +
                    s"(inferred as ${string(multiple)}, " +
                    s"but can be specified as ${string(potential)})")

              case (potential, Some(stated))
                  if potential > stated =>
                c.abort(typeDef.pos,
                  s"connection for ${peer.name} specified as " +
                  s"${string(stated)}, but must be ${string(potential)}")

              case _ =>
            }
          }

          multiplicities map { case (_, peer, multiplicity) =>
            PeerConnectionMultiplicity(definition.peerSymbol, peer,
              multiplicity match {
                case `single` => trees.SingleConnection
                case `optional` => trees.OptionalConnection
                case `multiple` => trees.MultipleConnection
              })
          }
      }

      multiplicities getOrElse List.empty
    }

    echo(verbose = true,
      s"  [${multiplicities.size} peer connection multiplicities added]")

    aggregator add multiplicities
  }
}
