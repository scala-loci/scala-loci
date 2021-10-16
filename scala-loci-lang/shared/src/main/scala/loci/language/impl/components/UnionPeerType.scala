package loci.language.impl.components

import loci.language.impl.Component
import loci.language.impl.Engine
import loci.language.impl.Phase

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
      children: Set[Peer],
      typeRef: Type,
    )

    /**
     * Traverse the body of the module and create one SyntheticPeergroup for each distinct union peer type.
     * For different occurrences of the same union peer type with different order of the types, e.g. `A | B` and
     * `B | A`, only one SyntheticPeergroup is created.
     */
    def collectSyntheticPeergroups(trees: List[Tree]): Seq[SyntheticPeergroup] = {
      val syntheticPeergroups = mutable.ListBuffer.empty[SyntheticPeergroup]

      object SyntheticPeergroupCollector extends Traverser {
        override def traverse(tree: Tree): Unit = tree match {
          case tree: TypeTree if isUnionPeerType(tree) =>
            collectUnionedPeers(tree.tpe) match {
              case peers if syntheticPeergroups.exists(_.children == peers) =>
              case peers =>
                val syntheticName = TypeName(s"$$loci$$synthetic$$peergroup$$${syntheticPeergroups.size}")
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
          case tree => super.traverse(tree)
        }
      }

      SyntheticPeergroupCollector.traverseTrees(trees)
      syntheticPeergroups.toSeq
    }

    /**
     * Replace union types occurring anywhere within the potentially deep (nested) type `tpe` with the respective
     * generated type ref of the generated SyntheticPeergroup.
     *
     * Handles nested types in form of typeArgs. Also handles the resultType of MethodType.
     */
    def replaceUnionTypesWithSyntheticPeergroupTypes(tpe: Type, syntheticPeergroups: Seq[SyntheticPeergroup]): Type = {
      tpe match {
        case null => null
        case tpe if tpe real_<:< types.union =>
          val unionedPeers = collectUnionedPeers(tpe)
          val syntheticPeergroupType = syntheticPeergroups.collectFirst {
            case SyntheticPeergroup(_, _, children, typeRef) if children == unionedPeers => typeRef
          }.getOrElse(
            c.abort(NoPosition, s"No generated peergroup found for union peer type $tpe. This should never happen!")
          )
          syntheticPeergroupType
        case MethodType(params, resultType) =>
          internal.methodType(params, replaceUnionTypesWithSyntheticPeergroupTypes(resultType, syntheticPeergroups))
        case tpe =>
          val replacedTypeArgs = tpe.typeArgs.map(replaceUnionTypesWithSyntheticPeergroupTypes(_, syntheticPeergroups))
          tpe mapArgs { _ => replacedTypeArgs }
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
          if (tree.symbol != null) {
            internal.setInfo(tree.symbol, replaceUnionTypesWithSyntheticPeergroupTypes(tree.symbol.info, syntheticPeergroups))
          }
          internal.setType(tree, replaceUnionTypesWithSyntheticPeergroupTypes(tree.tpe, syntheticPeergroups))
          super.transform(tree)
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
    def refinePeerTypeUpperBound(peerTypeDef: TypeDef, peer: Peer, syntheticPeergroups: Seq[SyntheticPeergroup]): TypeDef = {
      val TypeDef(mods, name, tparams, rhs) = peerTypeDef: @unchecked
      val syntheticUpperBounds = syntheticPeergroups.collect {
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
        // and add the synthetic peergroup defintions to the body
        val bodyWithSyntheticPeergroups = transformUnionPeers(tree.impl.body, syntheticPeergroups) ++ syntheticPeergroups.map(_.definition)

        // refine the upper bounds of the peer type defintions in the module code
        val bodyWithRefinedPeerTypeUpperBounds = bodyWithSyntheticPeergroups.map {
          case tree: TypeDef =>
            checkPeerType(tree.symbol, tree.pos) match {
              case Some(peer) =>
                val refinedDef = refinePeerTypeUpperBound(tree, peer, syntheticPeergroups)
                // we have to remove the Peer from the cache, as its definition is potentially updated
                removeFromPeerCache(tree.symbol)
                refinedDef
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

  private def isUnionPeerType(tree: TypeTree): Boolean = tree.tpe real_<:< types.union

  private def collectUnionedPeers(tpe: Type): Set[Peer] = {
    tpe match {
      case union if union.typeArgs.size == 2 =>
        val List(left, right) = union.typeArgs: @unchecked
        collectUnionedPeers(left) ++ collectUnionedPeers(right)
      case singlePeer =>
        val peer: Peer = requirePeerType(singlePeer.typeSymbol, singlePeer.typeSymbol.pos)
        Set(peer)
    }

  }

  private val peergroupAnnotation =
    internal.setType(q"new ${types.peergroup}", types.peer)
}
