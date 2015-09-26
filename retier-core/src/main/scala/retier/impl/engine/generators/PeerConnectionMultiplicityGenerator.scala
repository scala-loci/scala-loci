package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait PeerConnectionMultiplicityGenerator { this: Generation =>
  val c: Context
  import c.universe._

  val generatePeerConnectionMultiplicities = AugmentedAggregation[
    PeerDefinition, PeerConnectionMultiplicity] {
      aggregator =>

    echo(verbose = true, " Generating peer Connection multiplicities")

    val connectionTypeName = names.connection.toTypeName

    val multiplicities = aggregator.all[PeerDefinition] flatMap { definition =>
      val ClassDef(_, _, _, Template(_, _, body)) = definition.tree

      val multiplicities = body collectFirst {
        case typeDef @ TypeDef(_, connectionTypeName, List(), rhs) =>
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
            import trees._

            if (connection.tpe <:< types.single)
              (q"$SingleConnection", connection)
            else if (connection.tpe <:< types.optional)
              (q"$OptionalConnection", connection)
            else if (connection.tpe <:< types.multiple)
              (q"$MultipleConnection", connection)
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

          val combinations = (multiplicities combinations 2 map { pair =>
            val Seq(multiplicity0, multiplicity1) = pair
            (multiplicity0, multiplicity1)
          }).toList

          combinations foreach { case ((_, peer0, _), (_, peer1, _)) =>
            if (peer0.tpe =:!= peer1.tpe &&
                peer0.tpe.typeSymbol == peer1.tpe.typeSymbol)
              c.abort(typeDef.pos,
                s"using distinct types referring to the same peer type " +
                s"is not allowed: $peer0 and $peer1")
          }

          combinations foreach { case ((_, peer0, _), (_, peer1, _)) =>
            if (peer0.tpe =:!= peer1.tpe &&
                peer0.tpe.typeSymbol.name == peer1.tpe.typeSymbol.name)
              c.abort(typeDef.pos,
                s"using distinct peer types of the same name " +
                s"is not allowed: $peer0 and $peer1")
          }

          combinations foreach {
            case ((connection0, peer0, _), (connection1, peer1, _)) =>
              if (peer0.tpe =:= peer1.tpe)
                c.abort(typeDef.pos,
                  s"multiple occurrences of same peer type " +
                  s"are not allowed: $connection0 and $connection1")
          }

          multiplicities map { case (_, peer, multiplicity) =>
            PeerConnectionMultiplicity(definition.peerSymbol, peer, multiplicity)
          }
      }

      multiplicities getOrElse List.empty
    }

    echo(verbose = true,
      s"  [${multiplicities.size} peer connection multiplicities added]")

    aggregator add multiplicities
  }
}
