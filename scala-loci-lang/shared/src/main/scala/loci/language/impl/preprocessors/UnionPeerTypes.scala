package loci.language.impl.preprocessors

import loci.language.impl.Preprocessor

import scala.collection.mutable
import scala.reflect.macros.blackbox

object UnionPeerTypes extends Preprocessor.Factory[UnionPeerTypes] {
  override def apply[C <: blackbox.Context](c: C): UnionPeerTypes[C] = new UnionPeerTypes[C](c)
}

class UnionPeerTypes[C <: blackbox.Context](val c: C) extends Preprocessor[C] {
  import c.universe._

  case class SyntheticPeergroup(
    name: TypeName,
    definition: Tree,
    children: Set[TypeName],
  )

  val syntheticPeergroupNamePrefix = "$loci$synthetic$peergroup"

  private def replaceUnionPeerTypesWithSyntheticPeergroups(
    trees: List[Tree],
    moduleName: TypeName
  ): (List[Tree], Seq[SyntheticPeergroup]) = {
    val syntheticPeergroups = mutable.ListBuffer.empty[SyntheticPeergroup]

    val peerTypeDefs = trees.collect {
      case typeDef: TypeDef if typeDef.mods.annotations.exists(isPeerOrPeergroupAnnotation) => typeDef
    }

    object SyntheticPeergroupCollector extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case tree: TypTree if isUnionPeerType(tree) =>
          val collectedUnionPeers = collectUnionPeers(tree)
          collectedUnionPeers.foreach(validatePeer(_, tree.pos, peerTypeDefs))
          if (!syntheticPeergroups.exists(_.children == collectedUnionPeers)) {
            // peergroupName contains the module name to avoid conflicts when mixing in multitier traits
            val peergroupName = TypeName(s"$syntheticPeergroupNamePrefix$$$moduleName$$${syntheticPeergroups.size}")
            val peergroupDef = definePeergroupType(peergroupName, tree.pos)
            syntheticPeergroups.append(SyntheticPeergroup(peergroupName, peergroupDef, collectedUnionPeers))
          }
          Ident(syntheticPeergroups.find(_.children == collectedUnionPeers).get.name)
        case tree: TypeTree if tree.original != null => internal.setOriginal(tree, super.transform(tree.original))
        case tree => super.transform(tree)
      }
    }

    SyntheticPeergroupCollector.transformTrees(trees) -> syntheticPeergroups.toSeq
  }

  private def isUnionPeerType(tree: TypTree): Boolean = tree match {
    case AppliedTypeTree(Ident(TypeName("$bar")), _) => true
    case _ => false
  }

  private def collectUnionPeers(tree: Tree): Set[TypeName] = tree match {
    case AppliedTypeTree(Ident(TypeName("$bar")), List(left, right)) =>
      collectUnionPeers(left) ++ collectUnionPeers(right)
    case Ident(name: TypeName) => Set(name)
    case tree => c.abort(tree.pos, s"Unexpected type tree $tree when parsing union peer type")
  }

  private def validatePeer(peer: TypeName, pos: Position, topLevelTypeDefs: List[TypeDef]): Unit = {
    if (!topLevelTypeDefs.exists(_.name == peer)) {
      c.abort(pos, s"$peer is not a top-level peer type")
    }
  }

  private def isPeerOrPeergroupAnnotation(annotation: Tree): Boolean = annotation match {
    case q"new peer()" | q"new peergroup()" => true
    case _ => false
  }

  private def definePeergroupType(name: TypeName, pos: Position): TypeDef = atPos(pos) {
    TypeDef(
      Modifiers(
        Flag.DEFERRED | Flag.SYNTHETIC,
        typeNames.EMPTY,
        List(Apply(Select(New(Ident(TypeName("peergroup"))), termNames.CONSTRUCTOR), List()))
      ),
      name,
      List(),
      TypeBoundsTree(EmptyTree, EmptyTree)
    )
  }

  private def refinePeerTypeUpperBounds(
    peerTypeDef: TypeDef,
    syntheticPeergroups: Seq[SyntheticPeergroup]
  ): TypeDef = {
    val TypeDef(mods, name, tparams, rhs) = peerTypeDef: @unchecked

    val syntheticUpperBounds = syntheticPeergroups.collect {
      case SyntheticPeergroup(peergroupName, _, children) if children.contains(name) => peergroupName
    }
    val transitivePeergroupUpperBounds = if (mods.hasFlag(Flag.SYNTHETIC)) {
      val ownChildren = syntheticPeergroups.collectFirst {
        case SyntheticPeergroup(peergroupName, _, children) if peergroupName == name => children
      }.get
      syntheticPeergroups.collect {
        case SyntheticPeergroup(peergroupName, _, children) if peergroupName != name && ownChildren.subsetOf(children) =>
          peergroupName
      }
    } else {
      Seq()
    }

    val refinedRhs = (syntheticUpperBounds ++ transitivePeergroupUpperBounds).map(Ident(_)).toList match {
      case List() => rhs
      case upperBounds => rhs match {
        case TypeBoundsTree(lo, hi) => TypeBoundsTree(lo, hi match {
          case EmptyTree => CompoundTypeTree(Template(upperBounds, noSelfType, List()))
          case bound @ Ident(_) => CompoundTypeTree(Template(bound +: upperBounds, noSelfType, List()))
          case CompoundTypeTree(Template(List(Select(Ident(_), TypeName("AnyRef"))), self, body)) =>
            CompoundTypeTree(Template(upperBounds, self, body))
          case CompoundTypeTree(Template(bounds, self, body)) =>
            CompoundTypeTree(Template(bounds ++ upperBounds, self, body))
        })
      }
    }
    TypeDef(mods, name, tparams, refinedRhs)
  }

  /**
   * Process the untyped multitier module, replacing union peer types with synthetically generated peergroups.
   *
   * For each occurrence of a union peer type `A | B | ...` a synthetic peergroup definition is added to the module
   * and the respective union peer type is replaced with a reference to this peergroup. Each distinct union peer type
   * only gets one synthetic peergroup. Union peer types are considered identical, if they only differ by order of the
   * peers in the union.
   *
   * Additionally, the upper bounds of the peer type defintions are refined. For a synthetic peergroup defined for
   * `A | B | ...` this peergroup is added as upper bound to the type defintions of `A, B, ...`. Additionally, if
   * one synthetic peergroup spans a subset of peers of another synthetic peergroup, it gets the other as upper bound.
   *
   * The union peer replacement happens in preprocessing rather than the processing phases, as this makes it easier to
   * replace the types, given that preprocessing works with the untyped tree, which means that we only have to replace
   * TypeTrees and not Types and Symbols, as we would in processing, where we work on typed trees.
   */
  override def process(tree: Tree): Tree = {
    val (treeWithSyntheticPeergroups, syntheticPeergroups) = tree match {
      case tree: ImplDef =>
        val (replacedBody, syntheticPeergroups) =
          replaceUnionPeerTypesWithSyntheticPeergroups(tree.impl.body, tree.name.toTypeName)
        tree.map {
          case (mods, parents, self, _) =>
            (mods, parents, self, replacedBody ++ syntheticPeergroups.map(_.definition))
        } -> syntheticPeergroups
      case _ => c.abort(tree.pos, "Multitier module needs to be an ImplDef")
    }
    val treeWithRefinedPeerTypeUpperBounds = treeWithSyntheticPeergroups.map {
      case (mods, parents, self, body) =>
        val refinedBody = body.map {
          case typeDef: TypeDef if typeDef.mods.annotations.exists(isPeerOrPeergroupAnnotation) =>
            refinePeerTypeUpperBounds(typeDef, syntheticPeergroups)
          case tree => tree
        }
        (mods, parents, self, refinedBody)
    }
    treeWithRefinedPeerTypeUpperBounds
  }

  implicit class ImplDefOps(tree: ImplDef) {
    def map(f: (Modifiers, List[Tree], ValDef, List[Tree]) =>
      (Modifiers, List[Tree], ValDef, List[Tree])): ImplDef = (tree: @unchecked) match {
      case ClassDef(mods, tpname, tparams, impl@Template(parents, self, body)) =>
        val (modsNew, parentsNew, selfNew, bodyNew) = f(mods, parents, self, body)
        treeCopy.ClassDef(tree, modsNew, tpname, tparams,
          treeCopy.Template(impl, parentsNew, selfNew, bodyNew))

      case ModuleDef(mods, tpname, impl@Template(parents, self, body)) =>
        val (modsNew, parentsNew, selfNew, bodyNew) = f(mods, parents, self, body)
        treeCopy.ModuleDef(tree, modsNew, tpname,
          treeCopy.Template(impl, parentsNew, selfNew, bodyNew))
    }
  }
}
