package loci.dev
package language
package impl

import scala.reflect.macros.blackbox.Context

trait Peers {
  val c: Context

  import c.universe._

  sealed trait Tie

  object Tie {
    val Multiple = new Tie { override def toString: String = "Multiple" }
    val Optional = new Tie { override def toString: String = "Optional" }
    val Single = new Tie { override def toString: String = "Single" }
  }

  case class Peer(
    symbol: Symbol, name: TypeName, uniqueName: TermName,
    bases: List[(Symbol, Tree)], ties: List[(Tie, Symbol, Tree)])

  object Peers {
    val MultipleTpe = typeOf[Multiple[_]]
    val OptionalTpe = typeOf[Optional[_]]
    val SingleTpe = typeOf[Single[_]]
    val PeerAnnotationTpe = typeOf[peer]
  }

  final class Peers(underExpansion: Set[Symbol]) {
    @inline def requirePeerType(symbol: Symbol): Peer =
      requirePeerType(symbol, EmptyTree, NoPosition)

    @inline def requirePeerType(symbol: Symbol, tree: Tree): Peer =
      requirePeerType(symbol, tree, NoPosition)

    @inline def requirePeerType(symbol: Symbol, pos: Position): Peer =
      requirePeerType(symbol, EmptyTree, pos)

    def requirePeerType(symbol: Symbol, tree: Tree, pos: Position): Peer = {
      val (symbolPos, symbolName) = info(symbol, tree, pos)

      val peer = checkPeerType(symbol, tree, pos) getOrElse c.abort(symbolPos,
        s"$symbolName is not a peer type: @peer type ${symbol.name}")

      if (!(underExpansion contains symbol) && (symbol.owner.info member peer.name) == NoSymbol)
        c.abort(symbolPos,
          s"no generated peer definition found for peer type $symbolName, " +
          s"maybe ${symbol.owner.fullName} is not multitier: @multitier ${symbol.owner}")

      peer
    }

    @inline def checkPeerType(symbol: Symbol): Option[Peer] =
      checkPeerType(symbol, EmptyTree, NoPosition)

    @inline def checkPeerType(symbol: Symbol, tree: Tree): Option[Peer] =
      checkPeerType(symbol, tree, NoPosition)

    @inline def checkPeerType(symbol: Symbol, pos: Position): Option[Peer] =
      checkPeerType(symbol, EmptyTree, pos)

    def checkPeerType(symbol: Symbol, tree: Tree, pos: Position): Option[Peer] = {
      // force loading of annotations
      val symbolType = symbol.info

      if (symbol.annotations exists { _.tree.tpe <:< Peers.PeerAnnotationTpe })
        Some(cache.getOrElse(symbol, {
          val (symbolPos, symbolName) = info(symbol, tree, pos)

          if (symbol.annotations.size != 1)
            c.abort(symbolPos,
              s"peer types cannot have annotations: $symbolName")

          // destruct base peers and tie specification
          val (basesSpec, tiesSpec) =
            typeByUpperBound(symbolType, tree, symbolPos, "peer types", symbolName) match {
              case (RefinedType(peerParents, peerDecls), tree) =>
                val (peerParentTrees, body) = tree.original match {
                  case CompoundTypeTree(Template(parents, _, body)) =>
                    parents -> body
                  case _ =>
                    List.fill(peerParents.size)(EmptyTree) -> List.empty
                }

                if (peerDecls.size == 1 && peerDecls.head.name == TypeName("Tie")) {
                  val tieTree = body match {
                    case List(TypeDef(_, _, _, tie)) => tie
                    case _ => EmptyTree
                  }
                  val tieSymbol = peerDecls.head
                  val tieType = tieSymbol.info
                  val tiePos = tieTree.pos orElse tieSymbol.pos orElse symbolPos

                  typeByUpperBound(tieType, tieTree, tiePos, "tie specifications", symbolName) match {
                    case (RefinedType(tieParents, tieDecls), tree) =>
                      val tieParentTrees = tree.original match {
                        case CompoundTypeTree(Template(parents, _, _)) =>
                          parents
                        case _ =>
                          List.fill(tieParents.size)(EmptyTree)
                      }

                      if (tieDecls.nonEmpty)
                        c.abort(tiePos,
                          s"tie specifications cannot have members: peer type $symbolName")
                      (peerParents zip peerParentTrees) -> (tieParents zip tieParentTrees)

                    case (tpe, tree) =>
                      (peerParents zip peerParentTrees) -> List(tpe -> tree)
                  }
                }
                else if (peerDecls.isEmpty)
                  (peerParents zip peerParentTrees) -> List.empty
                else
                  c.abort(symbolPos,
                    s"peers can only have a single type member { type Tie }: $symbolName")

              case (tpe, tree) =>
                List(tpe -> tree) -> List.empty
            }

          // ensure all base peers are peer types
          // and ignore base `Any` and `AnyRef`
          val bases = basesSpec collect {
            case (tpe, tree) if !(tpe =:= definitions.AnyTpe || tpe =:= definitions.AnyRefTpe) =>
              val symbol = tpe.typeSymbol
              requirePeerType(symbol, EmptyTree, tree.pos orElse symbolPos)
              (symbol, tree)
          }

          // ensure ties are specified to be `Multiple`, `Optional` or `Single`
          // and ignore tie `Any` and `AnyRef`
          val ties = tiesSpec collect {
            case (tpe, tree) if !(tpe =:= definitions.AnyTpe || tpe =:= definitions.AnyRefTpe) =>
              val multiplicity =
                if (tpe <:< Peers.MultipleTpe) Tie.Multiple
                else if (tpe <:< Peers.OptionalTpe) Tie.Optional
                else if (tpe <:< Peers.SingleTpe) Tie.Single
                else c.abort(tree.pos orElse symbolPos, s"illegal tie specification: $tpe")

              val tieTree = tree match {
                case AppliedTypeTree(_, List(arg)) => arg
                case _ => EmptyTree
              }

              (multiplicity, tpe.typeArgs.head.typeSymbol, tieTree)
          }

          // construct peer and add it to the cache
          // so we do not run checks again
          val name = TypeName(s"$$loci$$peer$$${symbol.name}")
          val uniqueName = TermName(s"$$loci$$peer$$${makeUniqueName(symbol)}")
          val peer = Peer(symbol, name, uniqueName, bases, ties)
          cache += symbol -> peer

          // ensure that tied types are peer types
          ties foreach { case (_, symbol, tree) =>
            requirePeerType(symbol, tree, tree.pos orElse symbolPos)
          }

          peer
        }))
      else
        None
    }

    private def typeByUpperBound(
        tpe: Type, tree: Tree, pos: Position,
        constructs: String, name: String): (Type, Tree) = tpe match {
      case TypeBounds(low, high) =>
        if (low =:= definitions.NothingTpe)
          high -> (tree.original match {
            case TypeBoundsTree(_, high) => high
            case _ => EmptyTree
          })
        else
          c.abort(pos, s"$constructs cannot have lower type bounds: $name")
      case _ =>
        c.abort(pos, s"$constructs cannot be type aliases: $name")
    }

    private def info(symbol: Symbol, tree: Tree, pos: Position): (Position, String) = {
      val symbolPos = tree.pos orElse symbol.pos orElse pos
      val name = symbol.fullName
      val index = name lastIndexOf "."
      val symbolName =
        if (index > 0) s"${name.substring(index + 1)} in ${name.substring(0, index)}"
        else name
      symbolPos -> symbolName
    }

    private def makeUniqueName(symbol: Symbol): String = {
      val owner = symbol.owner
      val name = symbol.name.toString

      if (owner == c.mirror.RootClass)
        name
      else if (symbol.isSynthetic)
        makeUniqueName(owner)
      else {
        val prefix = makeUniqueName(owner)
        val separator = if (owner.isType && !owner.isModuleClass) "$$$" else "$"
        val suffix = if (name endsWith termNames.LOCAL_SUFFIX_STRING) name.dropRight(1) else name
        s"$prefix$separator$suffix"
      }
    }

    private implicit class PositionOps(self: Position) {
      def orElse(other: Position) = if (self == NoPosition) other else self
    }

    private implicit class TreeOps(tree: Tree) {
      def original = tree match {
        case tree: TypeTree => tree.original
        case tree => tree
      }
    }

    private val cache = collection.mutable.Map.empty[Symbol, Peer]
  }
}
