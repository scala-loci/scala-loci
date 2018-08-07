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

  sealed trait Value
  case class PlacedValue(symbol: Symbol, tree: Tree,
    peer: Symbol, modality: Modality) extends Value
  case class NonPlacedValue(symbol: Symbol, tree: Tree) extends Value
  case class GlobalValue(symbol: Symbol, tree: Tree) extends Value

  def processPlacedValues(stats: List[Tree], peers: Peers): List[Value] = {
    // split statements into module-level and peer-level statements
    val values = stats flatMap {
      case tree: ValOrDefDef if !tree.symbol.isConstructor =>
        deconstructPlacementType(tree.symbol, tree.tpt) match {
          case Left(false) =>
            Seq(GlobalValue(tree.symbol, tree))

          case Left(true) =>
            Seq(
              NonPlacedValue(tree.symbol, tree),
              GlobalValue(tree.symbol, eraseValue(tree)))

          case Right((peer, tpe, tpt, modality)) =>
            Seq(
              PlacedValue(tree.symbol, processValue(tree, tpe, tpt), peer, modality),
              GlobalValue(tree.symbol, eraseValue(tree)))
        }

      case tree =>
        Seq(GlobalValue(tree.symbol, tree))
    }

    // validate types of placed values
    values foreach {
      case PlacedValue(symbol, _, peer, modality) =>
        // ensure value is placed on a peer
        peers.requirePeerType(peer, symbol.pos)

        // ensure the peer, on which the value is placed,
        // is defined in the same module
        if (!(symbol.owner.info.members exists { _ == peer }))
          c.abort(symbol.pos, s"$symbol cannot be placed on peer of another module")

        // ensure local placed values do not override non-local placed values
        // and placed values do not override non-placed values
        symbol.overrides foreach { overrideSymbol =>
          deconstructPlacementType(overrideSymbol, EmptyTree) match {
            case Right((_, _, _, overrideModality)) =>
              if (modality == Modality.Local && overrideModality != Modality.Local)
                c.abort(symbol.pos,
                  s"Local placed declaration ${symbol.fullNestedName} cannot override " +
                  s"non-local placed declaration ${overrideSymbol.fullNestedName}")

            case _ =>
              c.abort(symbol.pos,
                s"Placed declaration ${symbol.fullNestedName} cannot override " +
                s"non-placed declaration ${overrideSymbol.fullNestedName}")
          }
        }

      case _ =>
    }

    values
  }

  private def deconstructPlacementType(symbol: Symbol, tpt: Tree): Either[Boolean, (Symbol, Type, Tree, Modality)] = {
    if (symbol.isTerm && !symbol.isModule)
      symbol.info.finalResultType match {
        case TypeRef(_, symbols.on, List(valueType, peerType)) =>
          val valueTree = tpt.original match {
            case AppliedTypeTree(_, List(valueTree, _)) => valueTree
            case _ => EmptyTree
          }

          valueType match {
            case TypeRef(_, symbols.per, List(subjectiveValueType, subjectivePeerType)) =>
              val subjectiveValueTree = valueTree.original match {
                case AppliedTypeTree(_, List(subjectiveValueTree, _)) => subjectiveValueTree
                case _ => EmptyTree
              }

              Right((peerType.typeSymbol, subjectiveValueType, subjectiveValueTree,
                Modality.Subjective(subjectivePeerType)))

            case TypeRef(_, symbols.local, List(localValueType)) =>
              val localValueTree = valueTree.original match {
                case AppliedTypeTree(_, List(localValueTree)) => localValueTree
                case _ => EmptyTree
              }

              Right((peerType.typeSymbol, localValueType, localValueTree,
                Modality.Local))

            case _ =>
              Right((peerType.typeSymbol, valueType, valueTree,
                Modality.None))

          }
        case _ =>
          Left(true)
      }
    else
      Left(false)
  }

  private val compileTimeOnlyAnnotation = {
    val message = "Access to abstraction " +
      "only allowed on peers on which the abstraction is placed. " +
      "Remote access must be explicit."
    q"new ${trees.compileTimeOnly}($message)"
  }

  private def compileTimeOnly(mods: Modifiers) =
    mods mapAnnotations { compileTimeOnlyAnnotation +: _ }

  private def processValue(tree: ValOrDefDef, tpe: Type, tpt: Tree) =
    tree map { case (mods, name, _, rhs) =>
      (mods, name, tpt orElse TypeTree(tpe), rhs)
    }

  private def eraseValue(tree: ValOrDefDef) =
    if (!tree.symbol.isAbstract)
      tree map { case (mods, name, tpt, _) =>
        (compileTimeOnly(mods), name, tpt, q"null.asInstanceOf[$tpt]")
      }
    else
      tree map { case (mods, name, tpt, rhs) =>
        (compileTimeOnly(mods), name, tpt, rhs)
      }
}
