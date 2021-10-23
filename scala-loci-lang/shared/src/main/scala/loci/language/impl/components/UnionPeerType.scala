package loci.language.impl.components

import loci.language.impl.Component
import loci.language.impl.Engine
import loci.language.impl.Phase

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.macros.blackbox

object UnionPeerType extends Component.Factory[UnionPeerType](
  requires = Seq(Commons, Initialization, Peers, ModuleInfo)
) {
  override def asInstance[C <: blackbox.Context]: PartialFunction[Component[C], UnionPeerType[C]] = {
    case c: UnionPeerType[C] => c
  }

  override def apply[C <: blackbox.Context](engine: Engine[C]): UnionPeerType[C] = new UnionPeerType[C](engine)
}

class UnionPeerType[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {

  override val phases: Seq[Phase] = Seq(
    Phase(
      "unionpeer:group",
      groupUnionPeerTypes,
      after = Set("init:inst"),
      before = Set("*", "values:collect")
    )
  )

  private val commons = engine.require(Commons)
  private val initialization = engine.require(Initialization)
  private val peers = engine.require(Peers)
  private val moduleInfo = engine.require(ModuleInfo)

  import commons._
  import initialization._
  import peers._
  import moduleInfo._
  import engine.c
  import engine.c.universe._

  /**
   * This phase is executed before "values:collect".
   * It introduces synthetic peer type definitions for each occurrence of a union peer type `A | B | ...`
   * Each occurrence of such a union peer type in the code is then replaced with the generated synthetic type.
   */
  def groupUnionPeerTypes(records: List[Any]): List[Any] = {

    case class SyntheticPeergroup(
      name: TypeName,
      definition: Tree,
      children: Set[Symbol],
      typeRef: Type,
    )

    val syntheticPeergroupNamePrefix = "$loci$synthetic$peergroup"

    /**
     * Traverse the body of the module and create one SyntheticPeergroup for each distinct union peer type.
     * For different occurrences of the same union peer type with different order of the types, e.g. `A | B` and
     * `B | A`, only one SyntheticPeergroup is created.
     */
    def collectSyntheticPeergroups(trees: List[Tree]): Seq[SyntheticPeergroup] = {
      val syntheticPeergroups = mutable.ListBuffer.empty[SyntheticPeergroup]

      object SyntheticPeergroupCollector extends Traverser {
        override def traverse(tree: Tree): Unit = tree match {
          case tree: TypTree if isUnionPeerType(tree) =>
            collectUnionedPeers(tree.tpe) match {
              case peers if syntheticPeergroups.exists(_.children == peers) =>
              case peers =>
                // syntheticName contains the className to avoid conflicts when mixing in multitier traits
                val syntheticName = TypeName(s"$syntheticPeergroupNamePrefix$$${module.className.toString}$$${syntheticPeergroups.size}")
                val syntheticTypeSymbol = internal.newTypeSymbol(module.classSymbol, syntheticName, tree.pos, Flag.SYNTHETIC | Flag.DEFERRED)
                internal.setInfo(syntheticTypeSymbol, internal.typeBounds(definitions.NothingTpe, definitions.AnyTpe))
                internal.setAnnotations(syntheticTypeSymbol, Annotation.apply(peergroupAnnotation))

                val peergroupDef = atPos(tree.pos) {
                  internal.setSymbol(
                    q"${Flag.SYNTHETIC} type $syntheticName",
                    syntheticTypeSymbol
                  )
                }

                val typeRef = internal.typeRef(internal.thisType(module.classSymbol), syntheticTypeSymbol, List())

                syntheticPeergroups.append(SyntheticPeergroup(syntheticName, peergroupDef, peers, typeRef))
            }
          case tree: TypeTree if tree.original != null => super.traverse(tree.original)
          case tree => super.traverse(tree)
        }
      }

      SyntheticPeergroupCollector.traverseTrees(trees)
      syntheticPeergroups.toSeq
    }

    /**
     * Replace union types occurring anywhere within the potentially deep (nested) type `tpe` with the respective
     * generated type ref of the generated SyntheticPeergroup.
     */
    @tailrec
    def replaceUnionTypesWithSyntheticPeergroupTypes(tpe: Type, syntheticPeergroups: Seq[SyntheticPeergroup]): Type = {
      tpe match {
        case null => null
        case tpe =>
          // using 'find' to get the top-level union, as 'map' works bottom-up and therefore would try to replace
          // the bottom-level union (i.e. 2 peers) first
          val firstUnionType = tpe.find(_ real_<:< types.union)
          firstUnionType match {
            case Some(unionType) =>
              val tpeWithFirstUnionTypeReplaced = tpe.map {
                case tpePart if tpePart =:= unionType =>
                  val unionedPeers = collectUnionedPeers(tpePart)
                  val syntheticPeergroupType = syntheticPeergroups.collectFirst {
                    case SyntheticPeergroup(_, _, children, typeRef) if children == unionedPeers => typeRef
                  }.getOrElse(
                    c.abort(NoPosition, s"No generated peergroup found for union peer type $tpePart. This should never happen!")
                  )
                  syntheticPeergroupType
                case tpePart => tpePart
              }
              replaceUnionTypesWithSyntheticPeergroupTypes(tpeWithFirstUnionTypeReplaced, syntheticPeergroups)
            case None => tpe
          }
      }
    }

    /**
     * Transform the body of the module by replacing union types occurring anywhere in types within the tree with the
     * respective generated type refs of the generated SyntheticPeergroups.
     *
     * Handles both the tpe of the tree, as well as the info of the tree's symbol.
     */
    def transformUnionPeers(trees: List[Tree], syntheticPeergroups: Seq[SyntheticPeergroup]): List[Tree] = {
      object UnionPeerTypeTransformer extends Transformer {
        override def transform(tree: Tree): Tree = {
          val newType = replaceUnionTypesWithSyntheticPeergroupTypes(tree.tpe, syntheticPeergroups)
          val newInfo = tree.symbol match {
            case null => None
            case sym => Option(replaceUnionTypesWithSyntheticPeergroupTypes(sym.info, syntheticPeergroups))
          }

          if (newType != null && newType =:= tree.tpe && newInfo.forall(_ == tree.symbol.info)) {
            super.transform(tree)
          } else {
            tree match {
              case tree: TypTree if newType != null =>
                val newTree: Tree = createTypeTree(newType, tree.pos)
                internal.setType(newTree, newType)
                newTree
              case tree =>
                if (tree.symbol != null && tree.symbol != NoSymbol) {
                  newInfo.foreach(internal.setInfo(tree.symbol, _))
                }
                internal.setType(tree, newType)
                super.transform(tree)
            }
          }
        }
      }

      UnionPeerTypeTransformer.transformTrees(trees)
    }

    /**
     * Include the generated synthetic peergroup definitions into the peer type hierarchy by adding the necessary
     * upper bounds to the peer type definition.
     *
     * For instance the peers `A` and `B` both get an upper bound to the synthetic peergroup generated for `A | B`.
     * Also, the generated peergroup for `A | B` gets an upper bound to the synthetic peergroup generated for `A | B | C`.
     */
    def refinePeerTypeUpperBound(peerTypeDef: TypeDef, peer: Symbol, syntheticPeergroups: Seq[SyntheticPeergroup]): TypeDef = {
      val TypeDef(mods, name, tparams, rhs) = peerTypeDef: @unchecked

      // if a peer type overrides another peer type of a parent, we need to add the synthetic peergroups this overridden
      // peer type is subtype of to the overriding peer type as well
      val overriddenPeerTypesSyntheticUpperBounds = module.tree.impl.parents
        .filter(p => p.tpe =:!= definitions.AnyTpe && p.tpe =:!= definitions.AnyRefTpe: Boolean)
        .map(_.tpe.member(name))
        .collect {
          case sym if sym != NoSymbol && sym.isType && sym != peer => sym.info
        }
        .collect {
          case TypeBounds(_, RefinedType(parents, _)) => parents.filter { parent =>
            parent.typeSymbol.asType.isSynthetic && parent.typeSymbol.name.toString.contains(syntheticPeergroupNamePrefix)
          }
        }
        .flatten
        .map(_.asSeenFrom(module.classSymbol))

      val syntheticUpperBounds = overriddenPeerTypesSyntheticUpperBounds ++ syntheticPeergroups.collect {
        case SyntheticPeergroup(_, _, children, typeRef) if children.contains(peer) => typeRef
      }.toList ++ (if (peerTypeDef.symbol.isSynthetic) {
        val ownChildren = syntheticPeergroups.collectFirst {
          case SyntheticPeergroup(_, definition, children, _) if definition == peerTypeDef => children
        }.get
        syntheticPeergroups.collect {
          case SyntheticPeergroup(_, definition, children, typeRef)
            if definition != peerTypeDef && ownChildren.subsetOf(children) => typeRef
        }
      } else {
        List()
      })
      syntheticUpperBounds match {
        case List() => peerTypeDef
        case syntheticUpperBounds =>
          val refinedBounds = peerTypeDef.symbol.info match {
            case TypeBounds(lo, hi) => internal.typeBounds(lo, hi match {
              case RefinedType(parents, decls) => internal.refinedType(parents ++ syntheticUpperBounds, decls)
              case TypeRef(_, sym, _) if sym == definitions.AnyTpe.typeSymbol =>
                internal.refinedType(syntheticUpperBounds, internal.newScopeWith())
              case singleTypeRef: TypeRef =>
                internal.refinedType(singleTypeRef +: syntheticUpperBounds, internal.newScopeWith())
            })
          }
          val refinedRhs: Tree = createTypeTree(refinedBounds, peerTypeDef.pos)
          internal.setSymbol(
            TypeDef(mods, name, tparams, refinedRhs),
            internal.setInfo(peerTypeDef.symbol, refinedBounds)
          )
      }
    }

    records process {
      case Initialized(tree) =>
        // only collect and generate the necessary peergroups without changing the existing module code
        val syntheticPeergroups = collectSyntheticPeergroups(tree.impl.body)

        // replace union peer type occurrences with generated peergroup types in the existing module code
        // and add the synthetic peergroup definitions to the body
        val bodyWithSyntheticPeergroups = transformUnionPeers(tree.impl.body, syntheticPeergroups) ++ syntheticPeergroups.map(_.definition)

        // refine the upper bounds of the peer type definitions in the module code
        val bodyWithRefinedPeerTypeUpperBounds = bodyWithSyntheticPeergroups.map {
          case tree: TypeDef =>
            validatePeerType(tree.symbol, tree.pos) match {
              case Some(peer) => refinePeerTypeUpperBound(tree, peer, syntheticPeergroups)
              case None => tree
            }
          case tree => tree
        }

        val result = Initialized(ImplDefOps(tree).map { (mods, parents, self, body) =>
          (mods, parents, self, bodyWithRefinedPeerTypeUpperBounds)
        })

        // add the generated synthetic peergroup types to the module's scope
        def classInfoType(parents: List[Type], decls: Scope, typeSymbol: Symbol) = {
          val scope = internal.newScopeWith(decls.toSeq ++ (syntheticPeergroups map {
            _.definition.symbol
          }): _*)
          internal.classInfoType(parents, scope, typeSymbol)
        }

        val info = (module.classSymbol.info: @unchecked) match {
          case PolyType(typeParams, ClassInfoType(parents, decls, typeSymbol)) =>
            internal.polyType(typeParams, classInfoType(parents, decls, typeSymbol))
          case ClassInfoType(parents, decls, typeSymbol) =>
            classInfoType(parents, decls, typeSymbol)
        }

        internal.setInfo(module.classSymbol, info)

        result
    }
  }

  private def isUnionPeerType(tree: TypTree): Boolean = tree.tpe real_<:< types.union

  private def collectUnionedPeers(tpe: Type): Set[Symbol] = {
    tpe match {
      case union if union.typeArgs.size == 2 =>
        val List(left, right) = union.typeArgs: @unchecked
        collectUnionedPeers(left) ++ collectUnionedPeers(right)
      case singlePeer =>
        val peer = validatePeerType(singlePeer.typeSymbol, singlePeer.typeSymbol.pos)
          .getOrElse(c.abort(singlePeer.typeSymbol.pos, s"Expected peer type but found: ${singlePeer.typeSymbol}"))
        Set(peer)
    }

  }

  private val peergroupAnnotation =
    internal.setType(q"new ${types.peergroup}", types.peer)
}
