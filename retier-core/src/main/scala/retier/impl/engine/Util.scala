package retier
package impl
package engine

import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.TypecheckException

object Util {
  def apply[C <: Context](c: C): Util[c.type] =
    new Util[c.type](c)
}

class Util[C <: Context](val c: C) {
  import c.universe._
  import Flag._

  /**
   * Re-type-checks the given tree, i.e., first un-type-checks it and then
   * type-checks it again using [[untypecheck]] and [[typecheck]], respectively.
   */
  def retypecheck(tree: Tree): Tree =
    typecheck(untypecheck(tree))

  /**
   * Type-checks the given tree. If type-checking fails, aborts the macro
   * expansion issuing the type-checking error.
   */
  def typecheck(tree: Tree): Tree = {
    try c typecheck tree
    catch {
      case TypecheckException(pos, msg) =>
        c.abort(pos.asInstanceOf[Position], msg)
    }
  }

  /**
   * Un-type-checks the given tree.
   *
   * The type-checking process distorts certain ASTs (such as representations of
   * extractors, lazy values or case classes) in a way that they cannot be
   * type-checked again. The issue is described in
   * [[https://issues.scala-lang.org/browse/SI-5464 SI-5465]].
   *
   * This method tries to restore the AST to its original form, which can be
   * type-checked again, or abort the macro expansion if this is not possible.
   */
  def untypecheck(tree: Tree): Tree = {
    val possibleFlags = Seq(
      OVERRIDE, ABSTRACT, FINAL, SEALED, IMPLICIT, LAZY, PRIVATE, PROTECTED)

    object untypecheckFixer extends Transformer {
      override def transform(tree: Tree) = tree match {
        // fix extractors
        case UnApply(
            Apply(fun, List(Ident(TermName("<unapply-selector>")))), args) =>
          fun collect {
            case Select(fun, TermName("unapply" | "unapplySeq")) => fun
          } match {
            case fun :: _ =>
              super.transform(Apply(fun, args))
            case _ =>
              super.transform(tree)
          }

        // fix lazy vals
        case ValDef(_, _, _, _)
            if tree.symbol.isTerm && {
              val term = tree.symbol.asTerm
              term.isLazy && term.getter != NoSymbol
            } =>
          EmptyTree
        case DefDef(mods, name, _, _, tpt, rhs)
            if tree.symbol.isTerm && {
              val term = tree.symbol.asTerm
              term.isLazy && term.isGetter
            } =>
          rhs collect {
            case Assign(_, rhs) => rhs
          } match {
            case rhs :: _ =>
              val flags = possibleFlags.fold(NoFlags) { (flags, flag) =>
                if (mods hasFlag flag) flags | flag else flags
              }
              ValDef(
                Modifiers(
                  flags, mods.privateWithin, mods.annotations),
                  name, super.transform(tpt), super.transform(rhs))
            case _ =>
              c.abort(tree.pos, "unexpected typed tree for lazy val")
              EmptyTree
          }

        // abort on case class
        case ClassDef(mods, _, _, _)
            if mods hasFlag CASE =>
          c.abort(tree.pos, "case class not allowed inside macro application")
          EmptyTree

        case _ =>
          super.transform(tree)
      }
    }

    c untypecheck (untypecheckFixer transform tree)
  }
}
