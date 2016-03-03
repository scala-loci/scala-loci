package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait PeerImplementationGenerator { this: Generation =>
  val c: Context
  import c.universe._

  val generatePeerImplementations = UniformAggregation[
    EnclosingContext with PeerDefinition with NonPlacedStatement with
    PlacedStatement with PlacedAbstraction with PeerConnectionMultiplicity] {
      aggregator =>

    echo(verbose = true, " Generating peer implementations")

    val synthetic = Flag.SYNTHETIC
    val peerSymbols = aggregator.all[PeerDefinition] map { _.peerSymbol }

    class PlacedReferenceAdapter(peerSymbol: TypeSymbol) extends Transformer {
      val baseClasses = peerSymbol.toType.baseClasses filter {
        _.asType.toType <:< types.peer
      }
      val baseOwners = baseClasses map { baseClass =>
        if (baseClass.owner.isModule)
          baseClass.owner.asModule.moduleClass
        else
          baseClass.owner
      }

      def placedType(tpe: Type): Option[Type] =
        if (tpe != null && tpe.finalResultType <:< types.localOn &&
            (types.bottom forall { tpe.finalResultType <:!< _ }))
          Some(tpe.finalResultType)
        else
          None

      def placedType(tree: Tree): Option[Type] =
        placedType(tree.tpe) orElse {
          if (tree.symbol.isTerm && tree.symbol.asTerm.isAccessor)
            placedType(tree.symbol.asMethod.accessed.typeSignature)
          else
            None
        }

      override def transform(tree: Tree) = tree match {
        case tree: TypeTree if tree.original != null =>
          internal setOriginal (tree, transform(tree.original))

        case q"$_.$name" if !tree.isRetierSynthetic =>
          placedType(tree) match {
            case Some(tpe) =>
              val Seq(_, peerType) = tpe.underlying.typeArgs
              if (baseClasses contains peerType.typeSymbol)
                q"$name"
              else
                super.transform(tree)

            case _ =>
              if (baseOwners contains tree.symbol.owner)
                q"$name"
              else
                super.transform(tree)
          }

        case _ =>
          super.transform(tree)
      }
    }

    object multitierInterfaceProcessor extends Transformer {
      import names._

      override def transform(tree: Tree) = tree match {
        case tree if tree.symbol == symbols.running =>
          q"$system.$systemRunning"

        case tree if tree.symbol == symbols.terminate =>
          q"$system.$systemTerminate"

        case _ =>
          super.transform(tree)
      }
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

    def flagDefinition(mods: Modifiers, expr: Tree) =
      Modifiers(
        if (expr == EmptyTree) mods.flags | Flag.DEFERRED else mods.flags,
        mods.privateWithin, mods.annotations)

    def peerPlacedAbstractions(peerSymbol: TypeSymbol) =
      aggregator.all[PlacedAbstraction] filter {
        _.peerSymbol == peerSymbol
      }

    def peerConnectionMultiplicities(peerSymbol: TypeSymbol) =
      aggregator.all[PeerConnectionMultiplicity] filter {
        _.peerSymbol == peerSymbol
      }

    def peerPlacedStatements(peerSymbol: TypeSymbol,
        peerSymbols: List[TypeSymbol]) = {
      val hasExpandingParent =
        (peerSymbol.toType.baseClasses.tail intersect peerSymbols).nonEmpty

      val placedStats = aggregator.all[PlacedStatement] collect {
        case PlacedStatement(
            definition @ ValDef(mods, name, _, _),
            `peerSymbol`, exprType, Some(declTypeTree), _, expr, index) =>
          (internal setPos (
            new PlacedReferenceAdapter(peerSymbol) transform
              ValDef(flagDefinition(mods, expr), name,
                createDeclTypeTree(declTypeTree, exprType), expr),
            definition.pos),
          index)

        case PlacedStatement(
            definition @ DefDef(mods, name, tparams, vparamss, _, _),
            `peerSymbol`, exprType, Some(declTypeTree), _, expr, index) =>
          (internal setPos (
            new PlacedReferenceAdapter(peerSymbol) transform
              DefDef(flagDefinition(mods, expr), name, tparams, vparamss,
                createDeclTypeTree(declTypeTree, exprType), expr),
            definition.pos),
          index)

        case PlacedStatement(tree, `peerSymbol`, _, None, _, expr, index)
            if !(symbols.specialPlaced contains tree.symbol) =>
          (new PlacedReferenceAdapter(peerSymbol) transform expr, index)
      }

      val specialPlacedStats = aggregator.all[PlacedStatement] collect {
        case PlacedStatement(tree, `peerSymbol`, _, None, _, expr, index)
            if symbols.specialPlaced contains tree.symbol =>
          (symbols.specialPlaced(tree.symbol),
           new PlacedReferenceAdapter(peerSymbol) transform expr)
      } groupBy {
        case (name, _) => name
      } map { case (name, exprs) =>
        val stats = exprs map { case (_, expr) => expr }
        q"""override def $name() = {
          super.$name
          ..$stats
        }"""
      }

      val nonPlacedStats =
        if (hasExpandingParent)
          List.empty
        else
          aggregator.all[NonPlacedStatement] collect {
            case stat @ NonPlacedStatement(tree, index) if stat.isPeerBound =>
              (new PlacedReferenceAdapter(peerSymbol) transform tree, index)
          }

      if (hasExpandingParent) {
        val firstConstructorStatement = (placedStats filter {
          case (ValDef(mods, _, _, _), _) => !(mods hasFlag Flag.LAZY)
          case (DefDef(_, _, _, _, _, _), _) => false
          case (ModuleDef(_, _, _), _) => false
          case _ => true
        }).headOption map {
          case (_, index) => index
        }

        firstConstructorStatement match {
          case Some(firstConstructorIndex) =>
            val placedStats = aggregator.all[PlacedStatement] filter {
              case PlacedStatement(stat, otherPeerSymbol, _, _, _, _, index) =>
                peerSymbol.toType =:!= otherPeerSymbol.toType &&
                peerSymbol.toType <:< otherPeerSymbol.toType &&
                !stat.isRetierSynthetic &&
                index > firstConstructorIndex
            } map { _.tree }

            val nonPlacedStats = aggregator.all[NonPlacedStatement] filter {
              case NonPlacedStatement(stat, index) =>
                stat.symbol != NoSymbol &&
                !stat.symbol.isType &&
                stat.symbol.name != termNames.CONSTRUCTOR &&
                index > firstConstructorIndex
            } map { _.tree }

            (placedStats ++ nonPlacedStats) filter {
              case ValDef(mods, _, _, _) => !(mods hasFlag Flag.LAZY)
              case DefDef(mods, _, _, _, _, _) => false
              case ModuleDef(_, _, _) => false
              case _ => true
            } foreach { stat =>
              c.warning(stat.pos,
                "Expression will be evaluated before preceding expressions " +
                "that are placed on peer subtypes")
            }

          case _ =>
        }
      }

      val stats = specialPlacedStats ++
        ((placedStats ++ nonPlacedStats) sortBy {
          case (_, index) => index
        } map {
          case (stat, _) => stat
        })

      stats map multitierInterfaceProcessor.transform
    }

    def peerImplementationParents(parents: List[Tree]) = parents map {
      case parent if parent.tpe =:= types.peer =>
        trees.PeerImpl

      case parent @ tq"$_[..$tpts]" if parent.tpe <:< types.peer =>
        val impl = peerImplementationTree(parent, parent.tpe, peerSymbols)
        tq"$impl[..$tpts]"

      case parent =>
        parent
    }

    def processPeerCompanion(peerDefinition: PeerDefinition) = {
      val PeerDefinition(tree, peerSymbol, typeArgs, args, parents, mods,
        _, isClass, companion, _) = peerDefinition

      val peerName = peerSymbol.name
      val abstractions = peerPlacedAbstractions(peerSymbol)
      val statements = peerPlacedStatements(peerSymbol, peerSymbols)
      val implParents = peerImplementationParents(parents)

      val duplicateName = peerSymbol.toType.baseClasses filter {
        _.asType.toType <:< types.peer
      } groupBy {
        _.name
      } collectFirst {
        case (name, symbols) if symbols.size > 1 => name
      }

      if (duplicateName.nonEmpty)
        c.abort(tree.pos,
          s"inheritance from peer types of the same name " +
          s"is not allowed: ${duplicateName.get}")

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

      val peerImpl =
        if (isClass)
          q"""$syntheticMods class $implementation[..$typeArgs](...$args)
                  extends ..$implParents {
                $dispatchImpl
                ..$statements
          }"""
        else
          q"""$syntheticMods trait $implementation[..$typeArgs]
                  extends ..$implParents {
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
      val PeerDefinition(tree, peerSymbol, typeArgs, args, parents, mods, stats,
        isClass, _, _) = peerDefinition

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

      peerDefinition.copy(
        tree = internal setPos (internal setType (
          generatedTree, tree.tpe), tree.pos),
        stats = generatedStats)
    }

    val definitions = aggregator.all[PeerDefinition] map
      (processPeerCompanion _ compose processPeerDefinition _)

    echo(verbose = true,
      s"  [${definitions.size} peer definitions generated, existing replaced]")

    aggregator replace definitions
  }
}
