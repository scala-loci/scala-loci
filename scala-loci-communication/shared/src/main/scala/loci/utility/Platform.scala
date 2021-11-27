package loci
package utility

import scala.reflect.NameTransformer
import scala.reflect.macros.blackbox

object platform {
  def annotation(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val q"new $expr(...$exprss).macroTransform(...$_)" = c.macroApplication: @unchecked

    if (exprss.size != 1 || exprss.head.size != 1)
      c.abort(expr.pos, "wrong number of arguments")

    val untypedArg = exprss.head.head

    val q"..$_; $_($typedArg)" =
      c typecheck atPos(untypedArg.pos) {
        q"""def $$loci$$argument(cond: ${typeOf[Boolean]}): Unit =
          ${termNames.ROOTPKG}.scala.Predef.locally(cond)
        $$loci$$argument($untypedArg)"""
      }: @unchecked

    val arg = typedArg match {
      case q"$_ = $arg" => arg
      case _ => typedArg
    }

    val keep = evaluateBooleanExpr(c)(arg)

    annottees match {
      case ClassDef(_, tpname, _, _) :: companion =>
        val tree = if (keep) annottees.head else q"class $tpname"
        companion.headOption.fold(tree) { companion => q"$tree; $companion" }

      case ModuleDef(_, name, _) :: Nil =>
        if (keep) annottees.head else q"object $name"

      case (_: MemberDef) :: Nil =>
        if (keep) annottees.head else q"()"

      case _ =>
        c.abort(c.enclosingPosition,
          "platform annotation only applicable to classes, traits, objects or member definitions")
    }
  }

  def apply[T](c: blackbox.Context)(cond: c.Tree)(body: c.Tree): c.Tree = {
    import c.universe._
    if (evaluateBooleanExpr(c)(cond)) body else q"()"
  }

  def value[T](c: blackbox.Context)(selection: c.Tree*): c.Tree = {
    import c.universe._

    val unconditionalSelection = symbolOf[loci.platform.UnconditionalSelection]
    val conditionalSelection = symbolOf[loci.platform.Selection.type]
    val arrowAssoc = symbolOf[ArrowAssoc[_]]
    val tuple2 = symbolOf[Tuple2[_, _]]

    val selections = selection map {
      case q"$conditional[..$_]($selection)"
          if conditional.symbol.owner == conditionalSelection &&
             conditional.symbol.name.toString == "conditional" =>
        val conditionalExpr = selection match {
          case q"$arrow[..$_]($condition).->[..$_]($expr)"
              if arrow.symbol.isMethod &&
                 arrow.symbol.asMethod.returnType.typeSymbol == arrowAssoc =>
            Right(condition -> expr)
          case q"new $arrow[..$_]($condition).->[..$_]($expr)"
              if arrow.symbol == arrowAssoc =>
            Right(condition -> expr)
          case q"$pair[..$_]($condition, $expr)"
              if pair.symbol.isMethod &&
                 pair.symbol.asMethod.returnType.typeSymbol == tuple2 =>
            Right(condition -> expr)
          case q"new $pair[..$_]($condition, $expr)"
              if pair.symbol == tuple2 =>
            Right(condition -> expr)
          case q"($condition, $expr)" =>
            Right(condition -> expr)
          case _ =>
            Left(selection.pos)
        }

        compatibility.either.map(conditionalExpr) { case (condition, expr) =>
          evaluateBooleanExpr(c)(condition) -> expr
        }

      case q"$unconditional[..$_]($expr)"
          if unconditional.symbol.owner == unconditionalSelection &&
             unconditional.symbol.name.toString == "unconditional" =>
        Right(true -> expr)

      case selection =>
        Left(selection.pos)
    }

    selections foreach {
      case Left(pos) => c.abort(pos, "unexpected platform selection")
      case _ =>
    }

    (selections
      collectFirst { case Right((true, expr)) => expr }
      getOrElse c.abort(c.enclosingPosition, "no selection matches the current platform"))
  }

  private def evaluateBooleanExpr(c: blackbox.Context)(expr: c.Tree): Boolean = {
    import c.universe._

    val platform = symbolOf[loci.platform.type]

    def isStable(tree: Tree): Boolean = tree match {
      case Select(qualifier, _) if tree.symbol.isTerm && tree.symbol.asTerm.isStable =>
        isStable(qualifier)
      case Ident(_) if tree.symbol.isTerm && tree.symbol.asTerm.isStable =>
        true
      case Literal(_) =>
        true
      case _ =>
        false
    }

    expr match {
      case Apply(Select(qualifier, name), args) =>
        val operator = NameTransformer.decode(name.toString)
        val lhs = evaluateBooleanExpr(c)(qualifier)

        def rhs = {
          if (args.size != 1)
            c.abort(expr.pos, s"unexpected number of arguments for operator $operator: ${args.size}")
          evaluateBooleanExpr(c)(args.head)
        }

        operator match {
          case "==" => lhs == rhs
          case "!=" => lhs != rhs
          case "&" | "&&" => lhs & rhs
          case "|" | "||" => lhs | rhs
          case "^" => lhs ^ rhs
          case _ => c.abort(expr.pos, s"unknown operator: $operator")
        }

      case Select(qualifier, name) if expr.symbol.isTerm && !expr.symbol.asTerm.isStable =>
        val operator = NameTransformer.decode(name.toString)
        val value = evaluateBooleanExpr(c)(qualifier)

        if (operator != "unary_!")
          c.abort(expr.pos, s"unknown operator: $operator")

        !value

      case _
        if expr.symbol != null &&
           expr.symbol.isMethod &&
           expr.symbol.owner == platform &&
           expr.tpe != null &&
           expr.tpe <:< typeOf[Boolean] &&
           isStable(expr) =>
        try
          loci.platform.getClass.getMethod(expr.symbol.name.toString).invoke(loci.platform).asInstanceOf[Boolean]
        catch {
          case _: NoSuchMethodException | _: IllegalArgumentException | _: ClassCastException =>
            c.abort(expr.pos, s"failed to read value: $expr")
        }

      case _ if isStable(expr) =>
        if (!(expr.tpe <:< typeOf[Boolean]))
          c.abort(expr.pos, s"not a boolean expression: $expr")

        expr.tpe match {
          case ConstantType(Constant(value: Boolean)) =>
            value
          case _ =>
            c.abort(expr.pos, s"constant value not known at compile time: $expr")
        }

      case _ =>
        c.abort(expr.pos, s"not a constant expression: $expr")
    }
  }
}
