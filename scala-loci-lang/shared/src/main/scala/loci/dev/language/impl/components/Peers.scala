package loci.dev
package language
package impl
package components

import scala.reflect.macros.blackbox

object Peers extends Component.Factory[Peers](
    requires = Seq(ModuleInfo, Commons)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new Peers(engine)
  def asInstance[C <: blackbox.Context] = { case c: Peers[C] => c }
}

class Peers[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq.empty

  val moduleInfo = engine.require(ModuleInfo)
  val commons = engine.require(Commons)

  import engine._
  import engine.c.universe._
  import moduleInfo._
  import commons._


  type Tie = Tie.Value

  object Tie extends Enumeration {
    val Multiple, Optional, Single = Value
  }

  case class Peer(symbol: Symbol, name: TypeName,
    bases: List[Peer.Base], ties: List[Peer.Tie])

  object Peer {
    trait Base {
      val tpe: Type
      val tree: Tree
    }

    object Base {
      def unapply(base: Base) = Some((base.tpe, base.tree))
    }

    case class InheritedBase(tpe: Type, name: TypeName, tree: Tree) extends Base

    case class DelegatedBase(tpe: Type, id: String, name: TermName, tree: Tree) extends Base

    case class Tie(tpe: Type, multiplicity: Peers.this.Tie, tree: Tree)
  }


  private val cache = collection.mutable.Map.empty[Symbol, Peer]

  private val underExpansion = module.symbol.info.decls.toSet

  val modulePeers: Seq[Peer] =
    (module.stats flatMap {
      case tree @ q"$_ type $_[..$_] = $tpt" =>
        checkPeerType(tree.symbol, tpt, tree.pos)
      case _ =>
        None
    }) ++
    (module.symbol.info.members flatMap { symbol =>
      if (symbol.isType && (module.symbol.info decl symbol.name) == NoSymbol)
        checkPeerType(symbol, symbol.pos)
      else
        None
    })


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

    val symbolOwnerType = symbol.owner.info

    if (symbol.annotations exists { _.tree.tpe <:< types.peer }) {
      // recompute result if the peer symbol is currently under expansion and
      // we are given a tree to ensure the result contains the correct trees
      if (!tree.isEmpty && (underExpansion contains symbol))
        cache -= symbol

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

              if (peerDecls.size == 1 && peerDecls.head.name == names.tie) {
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
          case (tpe, tree) if tpe =:!= definitions.AnyTpe && tpe =:!= definitions.AnyRefTpe =>
            val symbol = tpe.typeSymbol
            if (!(cache contains symbol))
              requirePeerType(symbol, EmptyTree, tree.pos orElse symbolPos)

            val id = uniqueName(tpe, symbolOwnerType)
            if (symbolOwnerType.members exists { _ == symbol })
              Peer.InheritedBase(tpe, TypeName(s"$$loci$$peer$$$id"), tree)
            else
              Peer.DelegatedBase(tpe, id, TermName(s"$$loci$$peer$$$id"), tree)
        }

        // ensure ties are specified to be `Multiple`, `Optional` or `Single`
        // and ignore tie `Any` and `AnyRef`
        val ties = tiesSpec collect {
          case (tpe, tree) if tpe =:!= definitions.AnyTpe && tpe =:!= definitions.AnyRefTpe =>
            val multiplicity =
              if (tpe <:< types.single) Tie.Single
              else if (tpe <:< types.optional) Tie.Optional
              else if (tpe <:< types.multiple) Tie.Multiple
              else c.abort(tree.pos orElse symbolPos, s"illegal tie specification: $tpe")

            val tieTree = tree match {
              case AppliedTypeTree(_, List(arg)) => arg
              case _ => EmptyTree
            }

            Peer.Tie(tpe.typeArgs.head, multiplicity, tieTree)
        }

        // construct peer and add it to the cache
        // so we do not run checks again
        val name = TypeName(s"$$loci$$peer$$${symbol.name}")
        val peer = Peer(symbol, name, bases, ties)
        cache += symbol -> peer

        // ensure that tied types are peer types
        ties foreach { case Peer.Tie(tpe, _, tree) =>
          val symbol = tpe.typeSymbol
          if (!(cache contains symbol))
            requirePeerType(symbol, tree, tree.pos orElse symbolPos)
        }

        // validate peer declaration (super peers and tie specification)
        if (!tree.isEmpty)
          validate(peer, symbolPos)

        peer
      }))
    }
    else
      None
  }

  private def validate(peer: Peer, pos: Position): Unit = {
    // ensure that peers only appear once as super peer
    peer.bases combinations 2 foreach {
      case Seq(Peer.Base(tpe0, _), Peer.Base(tpe1, tree1)) =>
        if (tpe0 =:= tpe1)
          c.abort(tree1.pos orElse pos,
            s"peer type cannot appear multiple times as super peer: $tpe1")
    }


    // ensure peer bases and tied peers are not type projections,
    // singleton types or existential types
    def validateTypeTree(tree: Tree) = tree match {
      case SingletonTypeTree(_) => Some("singleton type")
      case SelectFromTypeTree(_, _) => Some("type projection")
      case ExistentialTypeTree(_, _) => Some("existential type")
      case _ => None
    }

    peer.bases foreach { case Peer.Base(_, tree) =>
      validateTypeTree(tree) foreach { desc =>
        c.abort(tree.pos orElse pos,
          s"peer type cannot be a subtype of $desc $tree")
      }
    }

    peer.ties foreach { case Peer.Tie(_, _, tree) =>
      validateTypeTree(tree) foreach { desc =>
        c.abort(tree.pos orElse pos,
          s"illegal tie specification with $desc $tree")
      }
    }


    // ensure peer ties conform to super peer ties
    def tieSymbol(symbol: Symbol) = symbol.info decl names.tie

    def tieType(symbol: Symbol) = tieSymbol(symbol).info match {
      case TypeBounds(_, high) => high
      case _ => definitions.AnyTpe
    }

    val peerTie = tieType(peer.symbol)

    peer.bases foreach { case Peer.Base(tpe, _) =>
      val TypeRef(pre, _, _) = tpe
      val base = tpe.typeSymbol
      val baseTie = tieType(base).asSeenFrom(pre, base.owner)

      if (!(peerTie <:< baseTie))
        c.abort(tieSymbol(peer.symbol).pos orElse pos,
          s"tie specification for peer ${peer.symbol.name} does not conform to " +
          s"tie specification for super peer ${base.fullName}: " +
          s"$peerTie is not a subtype of $baseTie")
    }


    // ensure peer ties are consistent regarding subtype relations between peers
    peer.ties foreach { case Peer.Tie(tpe0, multiplicity0, tree0) =>
      val subtypes = (peer.ties
        filter { case Peer.Tie(tpe1, _, _) => tpe1 <:< tpe0 }
        sortWith { case (Peer.Tie(tpe0, _, _), Peer.Tie(tpe1, _, _)) => tpe0 <:< tpe1 })

      if (subtypes.size > 1)
        subtypes sliding 2 foreach {
          case Seq(Peer.Tie(tpe1, multiplicity1, _), Peer.Tie(tpe2, multiplicity2, tree2)) =>
            if (tpe1 =:= tpe2) {
              if (multiplicity1 != multiplicity2)
                c.abort(tree2.pos orElse pos,
                  s"$multiplicity1 tie required for $tpe2 " +
                  s"when specified for same peer $tpe1")
            }
            else if (tpe1 <:< tpe2) {
              if (multiplicity1 < multiplicity2)
                c.abort(tree2.pos orElse pos,
                  s"$multiplicity1 tie required for $tpe2 " +
                  s"when specified for sub peer $tpe1")
            }
            else {
              if (multiplicity0 != Tie.Multiple)
                c.abort(tree0.pos orElse pos,
                  s"${Tie.Multiple} tie required for $tpe0 " +
                  s"since unrelated sub peers are tied: $tpe1 and $tpe2")
            }
        }
    }
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

  private def info(symbol: Symbol, tree: Tree, pos: Position): (Position, String) =
    (symbol.pos orElse tree.pos orElse pos) -> symbol.fullNestedName
}
