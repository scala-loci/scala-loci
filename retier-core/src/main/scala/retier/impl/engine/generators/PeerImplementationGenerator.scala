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
    PlacedStatement with PlacedAbstraction with
    PeerConnectionMultiplicity] {
      aggregator =>

    echo(verbose = true, " Generating peer implementations")

    val synthetic = Flag.SYNTHETIC
    val peerSymbols = aggregator.all[PeerDefinition] map { _.peerSymbol }
    val enclosingName = aggregator.all[EnclosingContext].head.name

    class SelfReferenceChanger(originalName: Name, name: TypeName)
        extends Transformer {
      val originalTypeName = originalName.toTypeName
      val originalTermName = originalName.toTermName

      def isPlaced(tpe: Type): Boolean =
        tpe != null && tpe.finalResultType <:< types.localOn

      def isPlaced(tree: Tree): Boolean =
        isPlaced(tree.tpe) ||
        (tree.symbol.isTerm && tree.symbol.asTerm.isAccessor &&
         isPlaced(tree.symbol.asMethod.accessed.typeSignature))

      override def transform(tree: Tree) = tree match {
        case tree: TypeTree if tree.original != null =>
          internal setOriginal (tree, transform(tree.original))
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

    def createDeclTypeTree(declTypeTree: Tree, exprType: Type) =
      if (types.bottom exists { exprType <:< _ })
        declTypeTree
      else if (types.controlledIssuedPlacing exists { exprType <:< _ }) {
        val Seq(peer, value) = declTypeTree.typeArgTrees
        tq"$peer => $value"
      }
      else if (types.issuedPlacing exists { exprType <:< _ }) {
        val Seq(_, value) = declTypeTree.typeArgTrees
        value
      }
      else
        declTypeTree

    def peerPlacedAbstractions(peerSymbol: TypeSymbol) =
      aggregator.all[PlacedAbstraction] filter {
        _.peerSymbol == peerSymbol
      }

    def peerConnectionMultiplicities(peerSymbol: TypeSymbol) =
      aggregator.all[PeerConnectionMultiplicity] filter {
        _.peerSymbol == peerSymbol
      }

    def peerPlacedStatements(peerSymbol: TypeSymbol) =
      aggregator.all[PlacedStatement] collect {
        case PlacedStatement(
            definition @ ValDef(mods, name, _, _),
            `peerSymbol`, exprType, Some(declTypeTree), _, expr) =>
          internal setPos (
            SelfReferenceChanger(enclosingName, names.implementation) transform
              ValDef(mods, name,
                createDeclTypeTree(declTypeTree, exprType), expr),
            definition.pos)

        case PlacedStatement(
            definition @ DefDef(mods, name, tparams, vparamss, _, _),
            `peerSymbol`, exprType, Some(declTypeTree), _, expr) =>
          internal setPos (
            SelfReferenceChanger(enclosingName, names.implementation) transform
              DefDef(mods, name, tparams, vparamss,
                createDeclTypeTree(declTypeTree, exprType), expr),
            definition.pos)

        case PlacedStatement(tree, `peerSymbol`, _, None, _, expr) =>
          SelfReferenceChanger(enclosingName, names.implementation) transform
            expr
      }

    def peerImplementationParents(parents: List[Tree]) = parents map {
      case parent if parent.tpe =:= types.peer =>
        trees.PeerImpl

      case parent @ tq"$expr.$tpnamePeer[..$tpts]"
          if parent.tpe <:< types.peer =>
        val impl = peerImplementationTree(parent, parent.tpe, peerSymbols)
        tq"$impl[..$tpts]"

      case parent =>
        parent
    }

    def processPeerCompanion(peerDefinition: PeerDefinition) = {
      val PeerDefinition(_, peerSymbol, typeArgs, args, parents, mods,
        _, isClass, companion) = peerDefinition

      val peerName = peerSymbol.name
      val abstractions = peerPlacedAbstractions(peerSymbol)
      val statements = peerPlacedStatements(peerSymbol)
      val implParents = peerImplementationParents(parents)

      import trees._
      import names._

      val syntheticMods = Modifiers(
        mods.flags | Flag.SYNTHETIC, mods.privateWithin, mods.annotations)

      val dispatchImpl =
        q"""$synthetic override def $dispatch(
                request: $String,
                id: $AbstractionId,
                ref: $AbstractionRef): $Try[$String] = {
              import _root_.retier.impl.AbstractionRef._
              import _root_.retier.impl.RemoteRef._
              id match {
                case ..${abstractions map { _.dispatchClause } }
                case _ => super.$dispatch(request, id, ref)
              }
            }
         """

      val systemImpl = q"$synthetic override lazy val $system = new $System"

      val peerImpl =
        if (isClass)
          q"""$syntheticMods class $implementation[..$typeArgs](...$args)
                  extends ..$implParents {
                $systemImpl
                $dispatchImpl
                ..$statements
          }"""
        else
          q"""$syntheticMods trait $implementation[..$typeArgs]
                  extends ..$implParents {
                $systemImpl
                $dispatchImpl
                ..$statements
          }"""

      val peerInterface =
        q"""$synthetic object $interface {
              ..${abstractions flatMap { _.interfaceDefinitions } }
        }"""

      val generatedCompanion = companion match {
        case Some(q"""$mods object $tname
                    extends { ..$earlydefns } with ..$parents { $self =>
                    ..$stats
                  }""") =>
          q"""$mods object $tname
                  extends { ..$earlydefns } with ..$parents { $self =>
                ${markRetierSynthetic(peerInterface)}
                ${markRetierSynthetic(peerImpl)}
                ..$stats
          }"""

        case _ =>
          markRetierSynthetic(
            q"""$synthetic object ${peerName.toTermName} {
                  $peerInterface
                  $peerImpl
            }""")
      }

      peerDefinition.copy(companion = Some(generatedCompanion))
    }

    def processPeerDefinition(peerDefinition: PeerDefinition) = {
      val PeerDefinition(_, peerSymbol, typeArgs, args, parents, mods,
        stats, isClass, _) = peerDefinition

      val peerName = peerSymbol.name
      val multiplicities = peerConnectionMultiplicities(peerSymbol)

      import trees._
      import names._

      stats foreach {
        case stat: ValOrDefDef if stat.name == connection =>
          c.abort(stat.pos,
            s"member of name `$connection` not allowed in peer definitions")
        case _ =>
      }

      val hasPeerParents = parents exists { parent =>
        parent.tpe <:< types.peer && parent.tpe =:!= types.peer
      }

      val peerMultiplicities = multiplicities map {
        case PeerConnectionMultiplicity( _, connectedPeer,
            connectionMultiplicity) =>
          q"($peerTypeOf[$connectedPeer], $connectionMultiplicity)"
      }

      val peerConnections =
        if (peerMultiplicities.nonEmpty || !hasPeerParents)
          q"$Map[$PeerType, $ConnectionMultiplicity](..$peerMultiplicities)"
        else
          q"super.$connection"

      val connectionImpl = q"$synthetic def $connection = $peerConnections"

      val generatedStats = markRetierSynthetic(connectionImpl) :: stats
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

      peerDefinition.copy(tree = generatedTree, stats = generatedStats)
    }

    val definitions = aggregator.all[PeerDefinition] map
      (processPeerCompanion _ compose processPeerDefinition _)

    echo(verbose = true,
      s"  [${definitions.size} peer definitions generated, existing replaced]")

    aggregator replace definitions
  }
}
