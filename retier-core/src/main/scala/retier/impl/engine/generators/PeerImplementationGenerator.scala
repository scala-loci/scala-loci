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

    val peerDefs = aggregator.all[PeerDefinition] map {
      case peerDef @ PeerDefinition(_, peerName, peerType, typeArgs, args,
                                    parents, mods, _, isClass, companion) =>
        val abstractions = peerPlacedAbstractions(peerType)
        val stats = peerPlacedStatements(peerType)
        val implParents = peerImplementationParents(parents)
        val interfaceParents = peerInterfaceParents(parents)

        import trees._
        import names._

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

        val peerImpl =
          if (isClass)
            q"""$mods class $peer[..$typeArgs](...$args)
                    extends ..$implParents {
                  $systemImpl
                  $dispatchImpl
                  ..$stats
            }"""
          else
            q"""$mods trait $peer[..$typeArgs]
                    extends ..$implParents {
                  $systemImpl
                  $dispatchImpl
                  ..$stats
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

        peerDef.copy(companion = Some(generatedCompanion))
    }

    echo(verbose = true,
      s"  [${peerDefs.size} peer definitions generated, existing replaced]")

    aggregator replace peerDefs
  }
}
