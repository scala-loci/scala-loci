package retier
package impl
package engine

import org.scalamacros.resetallattrs._
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.TypecheckException

object Typer {
  def apply[C <: Context](c: C): Typer[c.type] =
    new Typer[c.type](c)
}

class Typer[C <: Context](val c: C) {
  import c.universe._
  import Flag._

  /**
   * Re-type-checks the given tree, i.e., first un-type-checks it and then
   * type-checks it again using [[untypecheck]] and [[typecheck]], respectively.
   */
  def retypecheck(tree: Tree): Tree =
    typecheck(untypecheck(tree))

  /**
   * Re-type-checks the given tree resetting all symbols using the
   * `org.scalamacros.resetallattrs` library, i.e., first un-type-checks it and
   * then type-checks it again using [[untypecheckAll]] and [[typecheck]],
   * respectively.
   */
  def retypecheckAll(tree: Tree): Tree =
    typecheck(untypecheckAll(tree))

  /**
   * Type-checks the given tree. If type-checking fails, aborts the macro
   * expansion issuing the type-checking error.
   *
   * The type-checking process distorts certain ASTs (such as representations of
   * extractors, lazy values or case classes) in a way that they cannot be
   * type-checked again. The issue is described in
   * [[https://issues.scala-lang.org/browse/SI-5464 SI-5465]].
   *
   * This method tries to restore the AST to its original form, which can be
   * type-checked again.
   */
  def typecheck(tree: Tree): Tree = {
    try
      fixTypecheck(
        c typecheck (persistentTypeFixer transform tree),
        abortWhenUnfixable = false)
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
  def untypecheck(tree: Tree): Tree =
    c untypecheck fixTypecheck(tree, abortWhenUnfixable = true)

  /**
   * Un-type-checks the given tree resetting all symbols using the
   * `org.scalamacros.resetallattrs` library.
   *
   * The type-checking process distorts certain ASTs (such as representations of
   * extractors, lazy values or case classes) in a way that they cannot be
   * type-checked again. The issue is described in
   * [[https://issues.scala-lang.org/browse/SI-5464 SI-5465]].
   *
   * This method tries to restore the AST to its original form, which can be
   * type-checked again, or abort the macro expansion if this is not possible.
   */
  def untypecheckAll(tree: Tree): Tree =
    c resetAllAttrs fixTypecheck(tree, abortWhenUnfixable = true)

  /**
   * Creates a type tree which survives re-type-checking using [[retypecheck]]
   * or [[retypecheckAll]], i.e., the type is recovered when type-checking the
   * tree using [[typecheck]], even if the tree has been un-type-checked before.
   *
   * Such a type tree can be useful because it is not easily possible to create
   * an AST for a given type and synthetic type trees are created during
   * type-checking.
   */
  def createTypeTree(tpe: Type): TypeTree =
    internal updateAttachment (TypeTree(tpe), PersistentType(tpe))

  private case class PersistentType(tpe: Type)

  private object persistentTypeFixer extends Transformer {
    override def transform(tree: Tree) = tree match {
      case tree: TypeTree =>
        (internal attachments tree).get[PersistentType] foreach { persistent =>
          internal setType (tree, persistent.tpe)
          internal removeAttachment[PersistentType] tree
        }
        super.transform(tree)

      case _ =>
        super.transform(tree)
    }
  }

  private def fixTypecheck(tree: Tree, abortWhenUnfixable: Boolean): Tree = {
    val possibleFlags = Seq(
      ABSTRACT, FINAL, IMPLICIT, LAZY, LOCAL,
      OVERRIDE, PRIVATE, PROTECTED, SEALED)

    val rhss = (tree collect {
      case valDef @ ValDef(_, _, _, _) if valDef.symbol.isTerm =>
        val term = valDef.symbol.asTerm
        List(term.getter -> valDef, term.setter -> valDef)
    }).flatten.toMap - NoSymbol

    object typecheckFixer extends Transformer {
      override def transform(tree: Tree) = tree match {
        // fix extractors
        case UnApply(
            Apply(fun, List(Ident(TermName("<unapply-selector>")))), args) =>
          fun collect {
            case Select(fun, TermName("unapply" | "unapplySeq")) => fun
          } match {
            case fun :: _ =>
              val apply = Apply(fun, args)
              internal setType (apply, fun.tpe)
              internal setPos (apply, fun.pos)
              super.transform(apply)
            case _ =>
              super.transform(tree)
          }

        // fix vars, vals and lazy vals
        case ValDef(_, _, _, _)
            if tree.symbol.isTerm && {
              val term = tree.symbol.asTerm
              term.getter != NoSymbol &&
                (term.isLazy || (rhss contains term.getter))
            } =>
          EmptyTree

        // fix vars and vals
        case DefDef(_, _, _, _, _, _)
            if tree.symbol.isTerm && {
              val term = tree.symbol.asTerm
              term.isSetter && (rhss contains term)
            } =>
          EmptyTree
        case defDef @ DefDef(mods, name, _, _, tpt, _)
            if tree.symbol.isTerm && {
              val term = tree.symbol.asTerm
              !term.isLazy && term.isGetter && (rhss contains term)
            } =>
          val valDef = rhss(tree.symbol)
          val flags = possibleFlags.fold(NoFlags) { (flags, flag) =>
            if (mods hasFlag flag) flags | flag else flags
          } | (if (valDef.symbol.asTerm.isVar) MUTABLE else NoFlags)
          val privateWithin =
            if (defDef.symbol.asTerm.privateWithin != NoSymbol)
              defDef.symbol.asTerm.privateWithin.name
            else
              mods.privateWithin
          val newValDef = ValDef(
            Modifiers(
              flags, privateWithin, mods.annotations),
              name, super.transform(valDef.tpt), super.transform(valDef.rhs))
          internal setType (newValDef, valDef.tpe)
          internal setPos (newValDef, valDef.pos)

        // fix lazy vals
        case defDef @ DefDef(mods, name, _, _, tpt, rhs)
            if tree.symbol.isTerm && {
              val term = tree.symbol.asTerm
              term.isLazy && term.isGetter
            } =>
          val assignment = rhs collect {
            case Assign(_, rhs) => rhs
          } match {
            case rhs :: _ => rhs
            case _ => rhs
          }
          val flags = possibleFlags.fold(NoFlags) { (flags, flag) =>
            if (mods hasFlag flag) flags | flag else flags
          }
          val privateWithin =
            if (defDef.symbol.asTerm.privateWithin != NoSymbol)
              defDef.symbol.asTerm.privateWithin.name
            else
              mods.privateWithin
          val valDef = rhss get tree.symbol
          val typeTree = valDef map { _.tpt } getOrElse tpt
          val newValDef = ValDef(
            Modifiers(
              flags, privateWithin, mods.annotations),
              name, super.transform(typeTree), super.transform(assignment))
          valDef map { valDef =>
            internal setType (newValDef, valDef.tpe)
            internal setPos (newValDef, valDef.pos)
          } getOrElse newValDef

        // abort on case class
        case ClassDef(mods, _, _, _)
            if (mods hasFlag CASE) && abortWhenUnfixable =>
          c.abort(tree.pos, "case class not allowed inside macro application")
          EmptyTree

        case _ =>
          super.transform(tree)
      }
    }

    typecheckFixer transform tree
  }
}
