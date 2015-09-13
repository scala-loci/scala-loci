package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait PeerImplementationGenerator { this: Generation =>
  val c: Context
  import c.universe._

  val generatePeerImplementations = UniformAggregation[
    EnclosingContext with PeerDefinition with
    PlacedStatement with PlacedAbstraction] {
      aggregator =>

    echo(verbose = true, " Generating peer implementations")

    val synthetic = Flag.SYNTHETIC
    val peerTypes = aggregator.all[PeerDefinition] map { _.peerType }
    val enclosingName = aggregator.all[EnclosingContext].head.name
    val connectionName = names.connection.toTypeName


    class SelfReferenceChanger(originalName: Name, name: TypeName)
        extends Transformer {
      val originalTypeName = originalName.toTypeName
      val originalTermName = originalName.toTermName

      def isPlaced(tpe: Type): Boolean = tpe <:< types.localOn

      def isPlaced(tree: Tree): Boolean =
        isPlaced(tree.tpe) ||
        (tree.symbol.isMethod &&
         isPlaced(tree.symbol.asMethod.accessed.typeSignature.finalResultType))

      override def transform(tree: Tree) = tree match {
        case ClassDef(_, `originalTypeName`, _, _) => tree
        case ModuleDef(_, `originalTermName`, _) => tree
        case Select(This(`originalTypeName`), _)
          if !tree.isRetierSynthetic && !isPlaced(tree) => tree
        case This(`originalTypeName`)
          if !tree.isRetierSynthetic => This(name)
        case _ => super.transform(tree)
      }
    }

    object SelfReferenceChanger {
      def apply(originalName: Name, name: Name) =
        new SelfReferenceChanger(originalName, name.toTypeName)
    }

    def peerPlacedAbstractions(peerType: Type) =
      aggregator.all[PlacedAbstraction] filter { _.peerType =:= peerType }

    def peerPlacedStatements(peerType: Type) =
      aggregator.all[PlacedStatement] collect {
        case PlacedStatement(
            definition @ ValDef(mods, name, _, _),
            `peerType`, _, Some(declTypeTree), _, expr) =>
          internal setPos (
            SelfReferenceChanger(enclosingName, names.peer) transform
              ValDef(mods, name, declTypeTree, expr),
            definition.pos)

        case PlacedStatement(
            definition @ DefDef(mods, name, tparams, vparamss, _, _),
            `peerType`, _, Some(declTypeTree), _, expr) =>
          internal setPos (
            SelfReferenceChanger(enclosingName, names.peer) transform
              DefDef(mods, name, tparams, vparamss, declTypeTree, expr),
            definition.pos)

        case PlacedStatement(tree, `peerType`, _, None, _, expr) =>
          SelfReferenceChanger(enclosingName, names.peer) transform expr
      }

    def peerImplementationParents(parents: List[Tree]) = parents map {
      case parent if parent.tpe =:= types.peer =>
        tq"retier.impl.PeerImpl"

      case parent @ tq"$expr.$tpnamePeer[..$_]"
          if parent.tpe <:< types.peer =>
        if (!(peerTypes exists { parent.tpe <:< _ })) {
          if ((parent.tpe.dealias.companion member names.peer) == NoSymbol)
            c.abort(parent.pos,
              "cannot access peer type implementation " +
              "(maybe peer definition was not placed " +
              "inside `multitier` environment)")
        }

        val name = tpnamePeer.toTermName
        tq"$expr.$name.${names.peer}"

      case parent =>
        parent
    }

    def peerInterfaceParents(parents: List[Tree]) = parents collect {
      case parent @ tq"$expr.$tpnamePeer[..$_]"
          if parent.tpe <:< types.peer && parent.tpe =:!= types.peer =>
        if (!(peerTypes exists { parent.tpe <:< _ })) {
          if ((parent.tpe.dealias.companion member names.interface) == NoSymbol)
            c.abort(parent.pos,
              "cannot access peer type interface " +
              "(maybe peer definition was not placed " +
              "inside `multitier` environment)")
        }

        val name = tpnamePeer.toTermName
        tq"$expr.$name.${names.interface.toTypeName}"
    }

    def peerConnections(tree: Tree) = tree match {
      case ClassDef(_, _, _, Template(_, _, body)) =>
        body collectFirst {
          case typeDef @ TypeDef(_, connectionName, List(), rhs) =>
            val connection = rhs.originalTree match {
              case TypeBoundsTree(lo, _) if lo.tpe =:!= typeOf[Nothing] =>
                c.abort(lo.pos,
                  "lower type bounds not allowed for connection type")
              case TypeBoundsTree(_, hi) => hi
              case _ => rhs
            }

            val connections = connection.originalTree match {
              case CompoundTypeTree(Template(treeParents, noSelfType, List())) =>
                val RefinedType(tpeParents, _) = connection.tpe
                treeParents zip tpeParents map { case (tree, tpe) =>
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

                if (argType <:!< types.peer || argType =:= typeOf[Nothing])
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

            import trees._

            val elements = multiplicities map { case (_, peer, multiplicity) =>
              val typeOfPeer = internal setPos (q"$peerTypeOf[$peer]", peer.pos)
              q"($typeOfPeer, $multiplicity)"
            }

            q"$Map[$PeerType, $ConnectionMultiplicity](..$elements)"
        }

      case _ =>
        None
    }

    val peerDefs = aggregator.all[PeerDefinition] map {
      case peerDef @ PeerDefinition(tree, peerName, peerType, typeArgs, args,
                                    parents, mods, stats, isClass, companion) =>
        val abstractions = peerPlacedAbstractions(peerType)
        val statements = peerPlacedStatements(peerType)
        val implParents = peerImplementationParents(parents)
        val interfaceParents = peerInterfaceParents(parents)

        import trees._
        import names._

        val connectionMultiplicity = peerConnections(tree) getOrElse {
          if (interfaceParents.nonEmpty)
            q"super.$connection"
          else
            q"$Map.empty[$PeerType, $ConnectionMultiplicity]"
        }

        val dispatchImpl =
          q"""override def $dispatch(
                  request: $String,
                  id: $AbstractionId,
                  ref: $AbstractionRef): $Try[$String] =
                id match {
                  case ..${abstractions map { _.dispatchClause } }
                  case _ => super.$dispatch(request, id, ref)
                }
           """

        val systemImpl =
          q"override lazy val $system = new $System"

        val connectionImpl =
          if (interfaceParents.nonEmpty)
            q"override def $connection = $connectionMultiplicity"
          else
            q"def $connection = $connectionMultiplicity"

        val peerImpl =
          if (isClass)
            q"""$mods class $peer[..$typeArgs](...$args)
                    extends ..$implParents {
                  $systemImpl
                  $dispatchImpl
                  ..$statements
            }"""
          else
            q"""$mods trait $peer[..$typeArgs]
                    extends ..$implParents {
                  $systemImpl
                  $dispatchImpl
                  ..$statements
            }"""

        val peerInterface =
          q"""trait ${interface.toTypeName} {
                ..${abstractions flatMap { _.interfaceDefinitions } }
          }"""

        val peerInterfaceObject =
          q"""object $interface
                extends ${interface.toTypeName} with ..$interfaceParents"""

        val generatedCompanion = companion match {
          case Some(q"""$mods object $tname
                      extends { ..$earlydefns } with ..$parents { $self =>
                      ..$stats
                    }""") =>
            q"""$mods object $tname
                    extends { ..$earlydefns } with ..$parents { $self =>
                  $peerInterface
                  $peerInterfaceObject
                  $peerImpl
                  ..$stats
            }"""

          case _ =>
            q"""$synthetic object ${peerName.toTermName} {
                  $peerInterface
                  $peerInterfaceObject
                  $peerImpl
            }"""
        }

        val generatedStats = connectionImpl :: stats
        val generatedTree =
          if (isClass)
            q"""$mods class $peerName[..$typeArgs](...$args)
                    extends ..$parents {
                  ..$generatedStats
            }"""
          else
            q"""$mods trait $peerName[..$typeArgs]
                    extends ..$parents {
                  ..$generatedStats
            }"""

        stats foreach {
          case stat: ValOrDefDef if stat.name == connection =>
            c.abort(stat.pos,
              s"member of name `$connection` not allowed in peer definitions")
          case _ =>
        }

        peerDef.copy(tree = generatedTree, stats = generatedStats, companion = Some(generatedCompanion))
    }

    echo(verbose = true,
      s"  [${peerDefs.size} peer definitions generated, existing replaced]")

    aggregator replace peerDefs
  }
}
