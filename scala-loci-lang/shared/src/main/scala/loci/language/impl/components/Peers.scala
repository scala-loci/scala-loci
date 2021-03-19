package loci
package language
package impl
package components

import scala.collection.mutable
import scala.reflect.NameTransformer
import scala.reflect.macros.blackbox

object Peers extends Component.Factory[Peers](
    requires = Seq(Commons, ModuleInfo)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new Peers(engine)
  def asInstance[C <: blackbox.Context] = { case c: Peers[C] => c }
}

class Peers[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq.empty

  val commons = engine.require(Commons)
  val moduleInfo = engine.require(ModuleInfo)

  import engine._
  import engine.c.universe._
  import commons._
  import moduleInfo._


  sealed trait Tie extends Ordered[Tie] {
    val key: Int
    def compare(that: Tie) = key - that.key
  }

  object Tie {
    case object Multiple extends Tie { val key = 0 }
    case object Optional extends Tie { val key = 1 }
    case object Single extends Tie { val key = 2 }

    def apply(key: Int): Tie = key match {
      case 0 => Multiple
      case 1 => Optional
      case 2 => Single
      case _ => throw new NoSuchElementException(s"key not found: $key")
    }
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

    case class InheritedBase(tpe: Type, tree: Tree) extends Base

    case class DelegatedBase(tpe: Type, tree: Tree) extends Base

    case class Tie(tpe: Type, multiplicity: Peers.this.Tie, tree: Tree)
  }


  def tieMultiplicity(ties: List[Peer.Tie], tpe: Type): Option[Tie] =
    ties.foldLeft(Option.empty[Int]) { (multiplicity, tie) =>
      if (tie.tpe =:= tpe)
        multiplicity map { _ max tie.multiplicity.key } orElse Some(tie.multiplicity.key)
      else if (tie.tpe <:< tpe)
        multiplicity orElse Some(Tie.Multiple.key)
      else
        multiplicity
    } map { Tie(_) }

  def tieMultiplicities(ties: List[Peer.Tie]): List[Peer.Tie] =
    ties.foldRight(List.empty[Peer.Tie]) { (tie, ties) =>
      val (updatedTies, foundTie) = ties.foldRight(List.empty[Peer.Tie] -> false) {
        case (existingTie, (ties, foundTie)) =>
          if (existingTie.tpe =:= tie.tpe) {
            if (tie.multiplicity > existingTie.multiplicity)
              ties -> foundTie
            else
              (existingTie :: ties) -> true
          }
          else
            (existingTie :: ties) -> foundTie
      }

      if (foundTie)
        updatedTies
      else
        tie :: updatedTies
    }


  private val readingRemoteAccesses: Map[Symbol, List[(Type, Position)]] = {
    val readingRemoteAccesses = mutable.Map.empty[Symbol, List[(Type, Position)]]
    val peerContext = mutable.ListBuffer.empty[(Symbol, String)]

    object traverser extends Traverser {
      override def traverse(tree: Tree): Unit = tree match {
        case q"$expr[..$_](...$exprss)"
            if exprss.nonEmpty &&
               exprss.head.nonEmpty &&
               tree.symbol != null &&
               (tree.symbol.owner == symbols.On ||
                tree.symbol.owner == symbols.Placed ||
                tree.symbol.owner == symbols.Block) =>
          val context = exprss.head.head.tpe.underlying.typeArgs.head
          val peer = context.underlying.typeArgs.head.typeSymbol

          val (name, pos) = exprss.head.head match {
            case Function(List(vparam), _) =>
              if (!(vparam.mods hasFlag Flag.IMPLICIT))
                c.abort(vparam.pos orElse tree.pos, "peer context parameter must be implicit")

              vparam.name.toString -> vparam.pos

            case _ =>
              "" -> NoPosition
          }

          peerContext.headOption foreach { case (_, outerName) =>
            if (outerName.nonEmpty && outerName != name)
              c.abort(pos orElse tree.pos,
                s"peer context parameter name `${NameTransformer decode name}` may not differ from " +
                s"outer peer context parameter name `${NameTransformer decode outerName}`")
          }

          traverse(expr)
          peerContext.prepend(peer -> name)
          traverseTreess(exprss)
          peerContext.remove(0)

        case q"$_[..$_](...$exprss)"
            if exprss.nonEmpty &&
               exprss.head.nonEmpty &&
               (tree.tpe real_<:< types.remoteAccessor) &&
               (exprss.head.head.tpe <:!< types.fromSingle) &&
               (exprss.head.head.tpe <:!< types.fromMultiple) =>
          peerContext.headOption foreach { case (outerPeer, _) =>
            val peer = exprss.head.head.tpe.finalResultType.widen.typeArgs(1)
            val accesses = readingRemoteAccesses.getOrElse(outerPeer, List.empty)
            readingRemoteAccesses.update(outerPeer, peer -> tree.pos :: accesses)
          }
          super.traverse(tree)

        case _ =>
          super.traverse(tree)
      }
    }

    traverser traverse module.tree

    readingRemoteAccesses.toMap
  }


  private val cache = mutable.Map.empty[Symbol, Peer]

  val modulePeers: Seq[Peer] =
    (module.tree.impl.body flatMap {
      case tree @ q"$_ type $_[..$_] = $tpt" =>
        checkPeerType(tree.symbol, tpt, tree.pos)
      case _ =>
        None
    }) ++
    (module.classSymbol.selfType.members flatMap { symbol =>
      if (symbol.isType && (module.symbol.info decl symbol.name) == NoSymbol)
        checkPeerType(symbol, symbol.pos)
      else
        None
    })

  def modulePeer(tpe: Type): Boolean = {
    val symbol = tpe.typeSymbol

    def isModuleSymbol(sym: Symbol) =
      sym == module.classSymbol ||
      sym.isModule && sym.asModule.moduleClass == module.classSymbol

    if (module.classSymbol.selfType.members.exists { _ == symbol })
      tpe match {
        case TypeRef(ThisType(sym), _, _) => isModuleSymbol(sym)
        case TypeRef(SingleType(_, sym), _, _) => isModuleSymbol(sym)
        case _ => false
      }
    else
      false
  }


  @inline def requirePeerType(symbol: Symbol): Peer =
    requirePeerType(symbol, EmptyTree, NoPosition)

  @inline def requirePeerType(symbol: Symbol, tree: Tree): Peer =
    requirePeerType(symbol, tree, NoPosition)

  @inline def requirePeerType(symbol: Symbol, pos: Position): Peer =
    requirePeerType(symbol, EmptyTree, pos)

  def requirePeerType(symbol: Symbol, tree: Tree, pos: Position): Peer = {
    val (symbolPos, symbolName) = info(symbol, tree, pos)

    val peer = checkPeerType(symbol, tree, pos) getOrElse c.abort(symbolPos, {
      if (symbol.name.toString == "<refinement>")
        s"${symbol.info} is not a peer type"
      else
        s"$symbolName is not a peer type: @peer type ${symbol.name}"
    })

    val name = TypeName(s"$$loci$$peer$$${uniqueName(symbol.owner, symbol.name.toString)}")
    if ((symbol.owner.info member name) == NoSymbol &&
        (!underEnclosingExpansion(symbol) ||
         !isMultitierModule(symbol.owner.info)))
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

    if (symbol.annotations exists { _.tree.tpe <:< types.peer }) {
      // recompute result if the peer symbol is currently under expansion and
      // we are given a tree to ensure the result contains the correct trees
      val cached =
        if (!tree.isEmpty && underExpansion(symbol)) {
          val cached = cache contains symbol
          cache -= symbol
          cached
        }
        else
          false

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

              val inheritedTies = peerParents exists { tpe =>
                tpe.typeSymbol.isClass && tpe.typeSymbol.isSynthetic
              }

              // extract tie specification from generated synthetic peer trait
              val peerDeclsInheritedTie =
                if (peerDecls.isEmpty && inheritedTies) {
                  val tie = symbolType member names.tie
                  if (tie != NoSymbol)
                    internal.newScopeWith(tie)
                  else
                    peerDecls
                }
                else
                  peerDecls

              if (peerDeclsInheritedTie.size == 1 && peerDeclsInheritedTie.head.name == names.tie) {
                val tieTree = body match {
                  case List(TypeDef(_, _, _, tie)) if !inheritedTies => tie
                  case _ => EmptyTree
                }
                val tieSymbol = peerDeclsInheritedTie.head
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
              else if (peerDeclsInheritedTie.isEmpty)
                (peerParents zip peerParentTrees) -> List.empty
              else
                c.abort(symbolPos,
                  s"peers can only have a single type member { type Tie }: $symbolName")

            case (tpe, tree) =>
              List(tpe -> tree) -> List.empty
          }

        // ensure all base peers are peer types
        // and ignore base `Any` and `AnyRef`
        // and generated synthetic peer traits
        val bases = basesSpec collect {
          case (tpe, tree)
              if tpe =:!= definitions.AnyTpe &&
                 tpe =:!= definitions.AnyRefTpe &&
                 tpe =:!= types.peerMarker &&
                 (!tpe.typeSymbol.isClass || !tpe.typeSymbol.isSynthetic) =>
            val symbol = tpe.typeSymbol

            if (!(cache contains symbol))
              requirePeerType(symbol, EmptyTree, tree.pos orElse symbolPos)

            val peerType = tpe.underlying.asSeenFrom(module.classSymbol)

            if (modulePeer(peerType))
              Peer.InheritedBase(peerType, tree)
            else
              Peer.DelegatedBase(peerType, tree)
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

            val tieType = tpe.typeArgs.head.underlying.asSeenFrom(module.classSymbol)

            Peer.Tie(tieType, multiplicity, tieTree)
        }

        // infer ties in case the peer is not explicitly overridden
        // but overridden peers come from (potentially multiple) super modules
        val inferredTies =
          if (tree.isEmpty &&
              symbol.owner != module.classSymbol &&
              (module.symbol.info.members exists { _ == symbol }))
            inferTies(symbol, ties, bases)
          else
            None

        val computedTies = tieMultiplicities(inferredTies getOrElse ties)

        // construct peer and add it to the cache
        // so we do not run checks again
        val name = TypeName(s"$$loci$$peer$$${uniqueName(module.symbol, symbol.name.toString)}")
        val peer = Peer(symbol, name, bases, computedTies)
        cache += symbol -> peer

        // ensure that tied types are peer types
        computedTies foreach { case Peer.Tie(tpe, _, tree) =>
          val symbol = tpe.typeSymbol
          if (!(cache contains symbol))
            requirePeerType(symbol, tree, tree.pos orElse symbolPos)
        }

        // validate peer declaration (super peers and tie specification)
        if ((module.symbol.info.members exists { _ == symbol }) && !cached)
          validate(peer, inferredTies.nonEmpty, symbolPos)

        peer
      }))
    }
    else
      None
  }

  private def validate(peer: Peer, tiesInferred: Boolean, pos: Position): Unit = {
    // ensure that peers only appear once as super peer
    peer.bases combinations 2 foreach { bases =>
      val Seq(Peer.Base(tpe0, _), Peer.Base(tpe1, tree1)) = bases: @unchecked
      if (tpe0 =:= tpe1)
        c.abort(tree1.pos orElse pos,
          s"peer type cannot appear multiple times as super peer: $tpe1")
    }


    // ensure that all super peers are defined in the same module
    peer.bases foreach { case Peer.Base(tpe, tree) =>
      if (!nestedPeer(tpe))
        c.abort(tree.pos orElse pos, s"peer type cannot declare a super peer of another module: $tpe")
    }


    // ensure peer bases and tied peers are not type projections,
    // singleton types or existential types
    def validateTypeTree(tree: Tree) = (tree collect {
      case SingletonTypeTree(_) => "singleton type"
      case SelectFromTypeTree(_, _) => "type projection"
      case ExistentialTypeTree(_, _) => "existential type"
    }).headOption

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


    // ensure peer bases conform to overridden peer bases
    val overridden = overriddenPeers(peer.symbol)

    overridden foreach { overridden =>
      overridden.bases foreach { base =>
        if (!(peer.bases exists { _.tpe <:< base.tpe })) {
          c.abort(pos,
            s"peer type needs to be a sub peer of ${base.tpe.typeSymbol.name} " +
            s"conforming to overridden peer ${overridden.symbol.nameInEnclosing}")
        }
      }
    }


    if (!tiesInferred) {
      // ensure peer ties conform to super and overridden peer ties
      def tieSymbol(symbol: Symbol) = symbol.info member names.tie

      def tieType(symbol: Symbol) = tieSymbol(symbol).info match {
        case TypeBounds(_, high) => high
        case _ => definitions.AnyTpe
      }

      val peerTie = tieType(peer.symbol).asSeenFrom(module.classSymbol)

      val peers =
        (peer.bases map { _.tpe -> "super" }) ++
        (overridden map { _.symbol.asType.toType -> "overridden" })

      peers foreach { case (tpe, relation) =>
        val TypeRef(pre, _, _) = tpe: @unchecked
        val base = tpe.typeSymbol
        val baseTie = tieType(base).asSeenFrom(pre, base.owner).asSeenFrom(module.classSymbol)

        if (!(peerTie <:< baseTie))
          c.abort(if (peer.symbol.owner == module.classSymbol) tieSymbol(peer.symbol).pos orElse pos else pos,
            s"tie specification of peer ${peer.symbol.name} does not conform to " +
            s"tie specification of $relation peer ${base.nameInEnclosing}: " +
            s"$peerTie is not a subtype of $baseTie")
      }


      // ensure peer ties are consistent regarding subtype relations between peers
      peer.ties foreach { case Peer.Tie(tpe0, multiplicity0, tree0) =>
        val subtypes = (peer.ties
          filter { case Peer.Tie(tpe1, _, _) => tpe1 <:< tpe0 }
          sortWith { case (Peer.Tie(tpe0, _, _), Peer.Tie(tpe1, _, _)) => tpe0 <:< tpe1 })

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
          case _ =>
        }
      }
    }
  }

  private def inferTies(symbol: Symbol, ties: List[Peer.Tie], bases: List[Peer.Base]): Option[List[Peer.Tie]] = {
    // collect tie specifications from base peers
    val baseTies = bases flatMap {
      case Peer.Base(TypeRef(pre, sym, _), _) =>
        requirePeerType(sym).ties map { tie =>
          tie.copy(tpe = tie.tpe.typeSymbol.ancestors.foldLeft(tie.tpe) { (tpe, symbol) =>
            tpe.asSeenFrom(pre, symbol)
          })
        }
      case _ =>
        List.empty
    }

    // merge tie specification of overridden peers
    val mergedTies = (overriddenPeers(symbol) map { _.ties }).foldLeft(ties ++ baseTies) { (ties0, ties1) =>
      val updatedTies0 = ties0 map { tie0 =>
        val multiplicity = ties1.foldLeft(tie0.multiplicity.key) { (multiplicity0, tie1) =>
          if (tie1.tpe =:= tie0.tpe)
            multiplicity0 max tie1.multiplicity.key
          else
            multiplicity0
        }
        tie0.copy(multiplicity = Tie(multiplicity))
      }

      val updatedTies1 = ties1 filterNot { tie1 =>
        ties0 exists { _.tpe =:= tie1.tpe }
      }

      updatedTies0 ++ updatedTies1
    }

    // check merged tie specification for consistency
    val tiesConsistent = mergedTies forall { tie =>
      val subtypes = (mergedTies
        filter { case Peer.Tie(tpe, _, _) => tpe <:< tie.tpe }
        sortWith { case (Peer.Tie(tpe0, _, _), Peer.Tie(tpe1, _, _)) => tpe0 <:< tpe1 })

      (subtypes sliding 2) forall {
        case Seq(Peer.Tie(tpe1, multiplicity1, _), Peer.Tie(tpe2, multiplicity2, tree2)) =>
          if (tpe1 =:= tpe2)
            multiplicity1 == multiplicity2 && tie.multiplicity.key >= multiplicity2.key
          else if (tpe1 <:< tpe2)
            multiplicity1 >= multiplicity2 && tie.multiplicity.key >= multiplicity2.key
          else
            tie.multiplicity == Tie.Multiple
        case _ =>
          true
      }
    }

    val moduleName = s"${module.name}.this."

    def peerName(tpe: Type) = {
      val typeName = tpe.toString
      if (typeName startsWith moduleName)
        typeName.substring(moduleName.length)
      else
        typeName
    }

    if (tiesConsistent) {
      // ensure newly inferred tie conforms to existing remote accesses
      // this is only relevant for reading remote accesses whose return value may depend on the tie
      readingRemoteAccesses get symbol foreach {
        _ foreach { case (tpe, pos) =>
          if (tieMultiplicity(ties, tpe) != tieMultiplicity(mergedTies, tpe)) {
            val ties = mergedTies map { tie =>
              val name = peerName(tie.tpe)
              tie.multiplicity match {
                case Tie.Single => s"Single[$name]"
                case Tie.Optional => s"Optional[$name]"
                case Tie.Multiple => s"Multiple[$name]"
              }
            }

            c.abort(pos,
              s"Inconsistent remote access for inferred peer ties for ${symbol.name}. " +
              s"You may define @peer type ${symbol.name} <: { type Tie <: ${ties mkString " with "} }")
          }
        }
      }

      Some(mergedTies)
    }
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

  private def info(symbol: Symbol, tree: Tree, pos: Position) = {
    (tree.pos
      orElse {
        (module.tree.impl.parents
          collectFirst {
            case parent if parent.symbol.asType.toType.members exists { _ == symbol } =>
              parent.pos
          }
          getOrElse NoPosition)
      }
      orElse pos) -> symbol.nameInEnclosing
  }

  private def overriddenPeers(symbol: Symbol) =
    (module.tree.impl.parents collect {
      case parent
          if parent.tpe =:!= definitions.AnyTpe &&
             parent.tpe =:!= definitions.AnyRefTpe =>
        val sym = parent.tpe member symbol.name
        if (sym != NoSymbol && sym != symbol)
          Some(requirePeerType(sym))
        else
          None
    }).flatten

  private def nestedPeer(tpe: Type): Boolean = {
    def isModuleSymbol(sym: Symbol) =
      sym == module.classSymbol ||
      sym.isModule && sym.asModule.moduleClass == module.classSymbol

    (tpe: @unchecked) match {
      case TypeRef(pre, _, _) =>
        isModuleSymbol(pre.typeSymbol) || isMultitierModule(pre) && nestedPeer(pre)
      case SingleType(pre, _) =>
        isMultitierModule(tpe) && nestedPeer(pre)
      case ThisType(sym) =>
        isModuleSymbol(sym)
    }
  }

  private def isMultitierModule(tpe: Type) =
    tpe.finalResultType.baseClasses take 2 exists { symbol =>
      symbol == module.symbol ||
      symbol == module.classSymbol ||
      (symbol.allAnnotations exists { _.tree.tpe <:< types.multitierModule })
    }
}
