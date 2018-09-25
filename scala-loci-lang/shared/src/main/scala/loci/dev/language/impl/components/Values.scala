package loci.dev
package language
package impl
package components

import scala.collection.mutable
import scala.reflect.macros.blackbox

object Values extends Component.Factory[Values](
    requires = Seq(ModuleInfo, Commons, Peers)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new Values(engine)
  def asInstance[C <: blackbox.Context] = { case c: Values[C] => c }
}

class Values[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq(
    Phase("values:collect", collectPlacedValues, before = Set("*", "values:validate")),
    Phase("values:validate", validatePlacedValues, before = Set("*")),
    Phase("values:fixrefs", fixEnclosingReferences, after = Set("*", "values:validate")))

  val moduleInfo = engine.require(ModuleInfo)
  val commons = engine.require(Commons)
  val peers = engine.require(Peers)

  import engine._
  import engine.c.universe._
  import moduleInfo._
  import commons._
  import peers._


  sealed trait Modality

  object Modality {
    case object None extends Modality
    case object Local extends Modality
    case class Subjective(peer: Type) extends Modality
  }

  sealed trait Value {
    val symbol: Symbol
    val owner: Symbol
    val tree: Tree

    def copy(symbol: Symbol = symbol, owner: Symbol = owner, tree: Tree = tree): Value = this match {
      case PlacedValue(_, _, _, peer, modality) => PlacedValue(symbol, owner, tree, peer, modality)
      case NonPlacedValue(_, _, _) => NonPlacedValue(symbol, owner, tree)
      case GlobalValue(_, _, _) => GlobalValue(symbol, owner, tree)
    }
  }

  sealed abstract class NonGlobalValue extends Value

  case class PlacedValue(symbol: Symbol, owner: Symbol, tree: Tree,
    peer: Symbol, modality: Modality) extends NonGlobalValue

  case class NonPlacedValue(symbol: Symbol, owner: Symbol, tree: Tree) extends NonGlobalValue

  case class GlobalValue(symbol: Symbol, owner: Symbol, tree: Tree) extends Value


  // split statements into module-level and peer-level statements
  def collectPlacedValues(records: List[Any]): List[Any] = {
    val values = module.stats flatMap {
      case tree: ValOrDefDef if tree.symbol.isTerm && !tree.symbol.isConstructor =>
        if (isMultitierModule(tree.tpt.tpe))
          Seq(GlobalValue(tree.symbol, module.symbol, tree))
        else
          destructPlacementType(tree.symbol.info, tree.tpt, tree.pos) match {
            case Some((peer, tpe, tpt, modality)) => Seq(
              PlacedValue(tree.symbol, module.symbol,
                changeType(stripPlacementSyntax(tree), tpe, tpt), peer, modality),
              GlobalValue(tree.symbol, module.symbol, eraseValue(tree)))

            case _ => Seq(
              NonPlacedValue(tree.symbol, module.symbol, tree),
              GlobalValue(tree.symbol, module.symbol, eraseValue(tree)))
          }

      case tree: MemberDef =>
        Seq(GlobalValue(tree.symbol, module.symbol, tree))

      case tree =>
        destructPlacementType(tree.tpe, EmptyTree, tree.pos) match {
          case Some((peer, _, _, modality)) =>
            Seq(PlacedValue(NoSymbol, module.symbol,
              stripPlacementSyntax(tree), peer, modality))

          case _ =>
            Seq(NonPlacedValue(NoSymbol, module.symbol, tree))
        }
    }

    records ++ values
  }

  // validate types of placed values
  def validatePlacedValues(records: List[Any]): List[Any] = {
    val placedSymbols = (records collect {
      case value: PlacedValue if value.symbol != NoSymbol => value.symbol
    }).toSet

    records foreach {
      case PlacedValue(symbol, owner, tree, peer, modality) =>
        // ensure value is placed on a peer
        requirePeerType(peer, tree.pos)

        // ensure the peer, on which the value is placed,
        // is defined in the same module
        if (!(owner.info.members exists { _ == peer }))
            c.abort(tree.pos, s"${if (symbol != NoSymbol) symbol else "Statement"} " +
              "cannot be placed on peer of another module")

        // ensure local placed values do not override non-local placed values
        // and placed values do not override non-placed values
        symbol.overrides foreach { overrideSymbol =>
          destructPlacementType(overrideSymbol.info, EmptyTree, overrideSymbol.pos orElse symbol.pos) match {
            case Some((_, _, _, overrideModality)) =>
              if (modality == Modality.Local && overrideModality != Modality.Local)
                c.abort(tree.pos,
                  s"Local placed declaration ${symbol.fullNestedName} cannot override " +
                  s"non-local placed declaration ${overrideSymbol.fullNestedName}")

            case _ =>
              c.abort(tree.pos,
                s"Placed declaration ${symbol.fullNestedName} cannot override " +
                s"non-placed declaration ${overrideSymbol.fullNestedName}")
          }
        }

        // ensure placed statements are neither subjective nor local
        if (symbol == NoSymbol)
          modality match {
            case Modality.Local =>
              c.abort(tree.pos, "Placed statements cannot be local")
            case Modality.Subjective(_) =>
              c.abort(tree.pos, "Placed statements cannot be subjective")
            case _ =>
          }

      case GlobalValue(_, _, _: ValOrDefDef) =>

      case value: GlobalValue =>
        value.tree foreach { tree =>
          if (placedSymbols contains tree.symbol)
            c.abort(tree.pos, accessMessage)
        }

      case _ =>
    }

    records
  }

  // fix self and super references to the enclosing module
  def fixEnclosingReferences(records: List[Any]): List[Any] =
    (records
      // fix references to expanding module that will be wrong
      // after moving the code to an inner trait
      process {
        case value: Value =>
          object transformer extends Transformer {
            override def transform(tree: Tree): Tree = tree match {
              // inferred type trees with no correspondence in the original source code
              case tree: TypeTree if tree.tpe != null && tree.original == null =>
                def hasOwner(symbol: Symbol): Boolean =
                  symbol != NoSymbol && (symbol == module.classSymbol || hasOwner(symbol.owner))

                val reInferType = tree.tpe exists {
                  case ThisType(sym) => hasOwner(sym)
                  case _ => false
                }

                if (reInferType)
                  TypeTree()
                else
                  tree

              // type trees with correspondence in the original source code
              case tree: TypeTree if tree.original != null =>
                internal.setOriginal(TypeTree(), transform(tree.original))

              // any other tree
              case _ =>
                super.transform(tree)
            }
          }

          value.copy(tree = transformer transform value.tree)
      }
      process {
        case value: NonGlobalValue =>
          object transformer extends Transformer {
            val skippedTrees = mutable.Set.empty[Tree]

            def skip(tree: Tree): Unit = tree match {
              case Select(qualifier, _) =>
                skippedTrees += tree
                skip(qualifier)
              case _ =>
                skippedTrees += tree
            }

            override def transform(tree: Tree): Tree = tree match {
              // type trees with correspondence in the original source code
              case tree: TypeTree if tree.original != null =>
                internal.setOriginal(TypeTree(), transform(tree.original))

              // reference to nested type (should not be transformed)
              case tree: RefTree if tree.isType =>
                skip(tree)
                super.transform(tree)

              // reference to nested term
              case tree: RefTree if !tree.symbol.isConstructor && !(skippedTrees contains tree) =>
                def superReference(tree: Tree): Boolean = tree match {
                  case Super(This(qual), typeNames.EMPTY)
                      if qual == typeNames.EMPTY || qual == module.classSymbol.name =>
                    true
                  case Super(_, _) =>
                    c.abort(tree.pos, "Static super references not supported in multitier code")
                  case tree if tree.children.nonEmpty =>
                    superReference(tree.children.head)
                  case _ =>
                    false
                }

                def stablePath(symbol: Symbol): Option[Tree] =
                  if (module.symbol.info.baseClasses contains symbol) {
                    if (superReference(tree))
                      Some(Super(This(names.placedValues), typeNames.EMPTY))
                    else
                      Some(This(names.placedValues))
                  }
                  else if (symbol.isModuleClass || symbol.isModule)
                    stablePath(symbol.owner) map { tree =>
                      val moduleSymbol = if(symbol.isClass) symbol.asClass.module else symbol
                      internal.setSymbol(Select(tree, symbol.name.toTermName), moduleSymbol)
                    }
                  else
                    None

                (stablePath(tree.symbol.owner)
                  map { treeCopy.Select(tree, _, tree.name) }
                  getOrElse super.transform(tree))

              // any other tree
              case _ =>
                super.transform(tree)
            }
          }

          value.copy(tree = transformer transform value.tree)
      })

  val moduleSelfReference =
    internal.setSymbol(Ident(names.multitierModule), module.classSymbol)

  def isMultitierModule(tpe: Type): Boolean = {
    val symbol = tpe.finalResultType.typeSymbol
    symbol == module.symbol ||
      symbol == module.classSymbol ||
      (symbol.allAnnotations exists { _.tree.tpe <:< types.multitierModule })
  }

  private def destructPlacementType(tpe: Type, tpt: Tree, pos: Position): Option[(Symbol, Type, Tree, Modality)] =
    tpe.finalResultType match {
      // placed value
      case TypeRef(_, symbols.on, List(valueType, peerType)) =>
        val valueTree = tpt.original match {
          case AppliedTypeTree(_, List(valueTree, _)) => valueTree
          case _ => EmptyTree
        }

        valueType match {
          // modality: subjective
          case TypeRef(_, symbols.per, List(subjectiveValueType, subjectivePeerType)) =>
            val subjectiveType =
              types.function mapArgs { _ => List(
                types.remote mapArgs { _ => List(subjectivePeerType) },
                subjectiveValueType)
              }

            val subjectiveTree = valueTree.original match {
              case AppliedTypeTree(_, List(subjectiveValueTree, subjectivePeerTree)) =>
                tq"${trees.remote}[$subjectivePeerTree] => $subjectiveValueTree"
              case _ => EmptyTree
            }

            validatePlacedType(subjectiveType, pos)
            Some((peerType.typeSymbol, subjectiveType, subjectiveTree,
              Modality.Subjective(subjectivePeerType)))

          // modality: subjective, but wrong syntax
          case tpe if tpe real_<:< types.subjective =>
            val Seq(value, peer) = extractTag(tpe, types.subjective, pos).typeArgs
            c.abort(pos, "Subjective placed type must be given as: " +
              s"${value.typeSymbol.name} per ${peer.typeSymbol.name}")

          // modality: local
          case TypeRef(_, symbols.local, List(localValueType)) =>
            val localValueTree = valueTree.original match {
              case AppliedTypeTree(_, List(localValueTree)) => localValueTree
              case _ => EmptyTree
            }

            validatePlacedType(localValueType, pos)
            Some((peerType.typeSymbol, localValueType, localValueTree,
              Modality.Local))

          // modality: none
          case _ =>
            validatePlacedType(valueType, pos)
            Some((peerType.typeSymbol, valueType, valueTree,
              Modality.None))

        }

      // non-placed value
      case tpe =>
        // wrong syntax for placed values
        if (tpe real_<:< types.placedValue) {
          val Seq(value, peer) = extractTag(tpe, types.placedValue, pos).typeArgs
          c.abort(pos, "Placed type must be given as: " +
            s"${value.typeSymbol.name} on ${peer.typeSymbol.name}")
        }

        validatePlacedType(tpe, pos)
        None
    }

  private def validatePlacedType(tpe: Type, pos: Position) = {
    val invalidType = tpe find {
      case TypeRef(_, symbols.local, _) => true
      case tpe => (tpe real_<:< types.placedValue) || (tpe real_<:< types.subjective)
    }

    invalidType foreach { invalidType =>
      c.abort(pos, s"Invalid nesting of placement types: $invalidType")
    }
  }

  private def extractTag(tpe: Type, tag: Type, pos: Position): Type = {
    val extractedTag = tpe.underlying match {
      case RefinedType(parents, _) =>
        val tags = parents filter { _ real_<:< tag }
        if (tags.size == 1)
          extractTag(tags.head, tag, pos)
        else
          NoType

      case tpe =>
        tpe
    }

    if (extractedTag real_<:!< tag)
      c.abort(pos, s"Could not find unique tag: $tag")

    extractedTag
  }

  private val accessMessage = "Access to abstraction " +
    "only allowed on peers on which the abstraction is placed. " +
    "Remote access must be explicit."

  private val accessAnnotation = q"new ${trees.compileTimeOnly}($accessMessage)"

  private val multitierStubAnnotation = q"new ${trees.multitierStub}"

  private def changeType(tree: ValOrDefDef, tpe: Type, tpt: Tree) =
    tree map { (mods, name, _, rhs) =>
      (mods, name, tpt orElse TypeTree(tpe), rhs)
    }

  private def eraseValue(tree: ValOrDefDef) =
    tree map { (mods, name, _, rhs) =>
      val tpt = createTypeTree(tree.tpt)
      (mods mapAnnotations { accessAnnotation :: multitierStubAnnotation :: _ },
        name, tpt,
        if (tree.symbol.isAbstract) rhs else q"null.asInstanceOf[$tpt]")
    }

  private def stripPlacementSyntax(tree: ValOrDefDef): ValOrDefDef =
    tree map { (mods, name, tpt, rhs) =>
      (mods, name, tpt, stripPlacementSyntax(rhs))
    }

  private def stripPlacementSyntax(tree: Tree): Tree = tree match {
    case q"$_[..$_](...$exprss)"
        if tree.nonEmpty &&
           (tree.symbol.owner == symbols.On ||
            tree.symbol.owner == symbols.Placed) =>
      val q"(..$_) => $expr" = exprss.head.head
      expr

    case _ =>
      tree
  }
}
