package loci.dev
package language
package impl

import scala.reflect.macros.blackbox.Context

trait Values { this: Definitions with Peers =>
  val c: Context

  import c.universe._

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

    def copy(symbol: Symbol = symbol, owner: Symbol = owner, tree: Tree = tree): Value =
      this match {
        case PlacedValue(_, _, _, peer, modality) => PlacedValue(symbol, owner, tree, peer, modality)
        case NonPlacedValue(_, _, _) => NonPlacedValue(symbol, owner, tree)
        case GlobalValue(_, _, _) => GlobalValue(symbol, owner, tree)
      }
  }

  case class PlacedValue(symbol: Symbol, owner: Symbol, tree: Tree,
    peer: Symbol, modality: Modality) extends Value
  case class NonPlacedValue(symbol: Symbol, owner: Symbol, tree: Tree) extends Value
  case class GlobalValue(symbol: Symbol, owner: Symbol, tree: Tree) extends Value

  def processValuePlacement(stats: List[Tree], owner: Symbol, peers: Peers): List[Value] = {
    val values = collectPlacedValues(stats, owner)
    validate(values, peers)
    fixEnclosingReferences(values)
  }

  // 1. split statements into module-level and peer-level statements
  private def collectPlacedValues(stats: List[Tree], owner: Symbol): List[Value] =
    stats flatMap {
      case tree: ValOrDefDef if tree.symbol.isTerm && !tree.symbol.isConstructor =>
        destructPlacementType(tree.symbol.info, tree.tpt, tree.pos) match {
          case Some((peer, tpe, tpt, modality)) => Seq(
            PlacedValue(tree.symbol, owner,
              changeType(stripPlacementSyntax(tree), tpe, tpt), peer, modality),
            GlobalValue(tree.symbol, owner, eraseValue(tree)))

          case _ => Seq(
            NonPlacedValue(tree.symbol, owner, tree),
            GlobalValue(tree.symbol, owner, eraseValue(tree)))
        }

      case tree if tree.symbol.isTerm && !tree.symbol.isConstructor && !tree.symbol.isModule =>
        destructPlacementType(tree.tpe, EmptyTree, tree.pos) match {
          case Some((peer, _, _, modality)) =>
            Seq(PlacedValue(NoSymbol, owner,
              stripPlacementSyntax(tree), peer, modality))

          case _ =>
            Seq(NonPlacedValue(NoSymbol, owner, tree))
        }

      case tree =>
        Seq(GlobalValue(tree.symbol, owner, tree))
    }

  // 2. validate types of placed values
  private def validate(values: List[Value], peers: Peers): Unit =
    values foreach {
      case PlacedValue(symbol, owner, tree, peer, modality) =>
        // ensure value is placed on a peer
        peers.requirePeerType(peer, tree.pos)

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

      case _ =>
    }

  // 3. fix self and super references to the enclosing module
  private def fixEnclosingReferences(values: List[Value]): List[Value] =
    values map {
      case value @ (PlacedValue(_, _, _, _, _) | NonPlacedValue(_, _, _)) =>
        val owner = value.symbol.owner
        val name = owner.name

        // fix references to expanding module that will be wrong
        // after moving the code to an inner trait
        object transformer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            // inferred type trees with no correspondence in the original source code
            case tree: TypeTree if tree.tpe != null && tree.original == null =>
              val reInferType = tree.tpe exists {
                case SingleType(ThisType(`owner`), _) => true
                case _ => false
              }

              if (reInferType)
                TypeTree()
              else
                tree

            // type trees with correspondence in the original source code
            case tree: TypeTree if tree.original != null =>
              internal.setOriginal(TypeTree(), transform(tree.original))

            // explicit self references
            case Select(This(`name`), name) =>
              Ident(name)

            // explicit reference to object through its path (accessors are also methods)
            case Select(_, name)
                if tree.symbol.isMethod && tree.symbol.owner == owner =>
              Ident(name)

            // static super reference
            case Super(_, mix)
                if mix != typeNames.EMPTY=>
              c.abort(tree.pos, "Static super references not supported in multitier code")

            // non-static super reference
            case Super(This(qual), typeNames.EMPTY)
                if qual == typeNames.EMPTY || qual == name =>
              Super(This(typeNames.EMPTY), typeNames.EMPTY)

            // any other tree
            case _ =>
              super.transform(tree)
          }
        }

        value.copy(tree = transformer transform value.tree)

      case value =>
        value
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

  private val compileTimeOnlyAnnotation = {
    val message = "Access to abstraction " +
      "only allowed on peers on which the abstraction is placed. " +
      "Remote access must be explicit."
    q"new ${trees.compileTimeOnly}($message)"
  }

  private def changeType(tree: ValOrDefDef, tpe: Type, tpt: Tree) =
    tree map { (mods, name, _, rhs) =>
      (mods, name, tpt orElse TypeTree(tpe), rhs)
    }

  private def eraseValue(tree: ValOrDefDef) =
    tree map { (mods, name, tpt, rhs) =>
      (mods mapAnnotations { compileTimeOnlyAnnotation :: _ },
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
