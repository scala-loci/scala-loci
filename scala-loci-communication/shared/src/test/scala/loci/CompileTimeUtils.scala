package loci

import scala.annotation.compileTimeOnly
import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.api.Position
import scala.reflect.macros.{ParseException, TypecheckException, whitebox}

object CompileTimeUtils {
  def replace(value: String, from: String, to: String): String =
    macro replaceImpl

  def replaceImpl(c: whitebox.Context)(value: c.Tree, from: c.Tree, to: c.Tree): c.Tree = {
    import c.universe._

    (value, from, to) match {
      case (Literal(Constant(value: String)), Literal(Constant(from: String)), Literal(Constant(to: String))) =>
        Literal(Constant(value.replace(from, to)))
      case _ =>
        c.abort(c.enclosingPosition, "string literal expected")
    }
  }

  def assertType[T](value: Any): Unit =
    macro assertTypeImpl[T]

  def assertTypeImpl[T: c.WeakTypeTag](c: whitebox.Context)(value: c.Tree): c.Tree = {
    import c.universe._

    val tpe = weakTypeOf[T]

    if (tpe =:= value.tpe)
      q"()"
    else
      failTest(c)(s"$value has type `${value.tpe}`; type `$tpe` expected")
  }

  def assertExactType[T](value: Any): Unit =
    macro assertExactTypeImpl[T]

  def assertExactTypeImpl[T: c.WeakTypeTag](c: whitebox.Context)(value: c.Tree): c.Tree = {
    import c.universe._

    val tpe = weakTypeOf[T]

    if (tpe == value.tpe)
      q"()"
    else
      failTest(c)(s"$value has type of form `${value.tpe}`; exact type `$tpe` expected")
  }

  def assertNoFailedAssertion(expr: String): Unit =
    macro assertNoFailedAssertionImpl

  def assertNoFailedAssertionImpl(c: whitebox.Context)(expr: c.Tree): c.Tree = {
    import c.universe._

    val testFailedException =
      c.mirror.staticClass(s"${termNames.ROOTPKG}.org.scalatest.exceptions.TestFailedException").asType.toType

    val messages = compileLiteralString(c)(expr) collect {
      case tree @ Apply(Select(New(_), _), args) if tree.tpe <:< testFailedException =>
        args match {
          case Function(_, Apply(_, List(Literal(Constant(message: String))))) :: _ => message
          case _ => "Assertion in compiled code failed"
        }
    }

    messages.headOption map { failTest(c)(_) } getOrElse q"()"
  }

  def abstractValuesInInstantiation(expr: String): List[String] =
    macro abstractValuesInInstantiationImpl

  def abstractValuesInInstantiationImpl(c: whitebox.Context)(expr: c.Tree): c.Tree = {
    import c.universe._

    val abstractValues = mutable.ListBuffer.empty[String]

    compileLiteralString(c)(expr) foreach {
      case New(tpt) =>
        val values = tpt.tpe.members collect {
          case symbol if symbol.isTerm && symbol.isAbstract =>
            symbol.name.decodedName.toString
        }
        abstractValues ++= values

      case _ =>
    }

    q"${abstractValues.result()}"
  }

  def containsCompileTimeOnly(expr: String): Boolean =
    macro containsCompileTimeOnlyImpl

  def containsCompileTimeOnlyImpl(c: whitebox.Context)(expr: c.Tree): c.Tree = {
    import c.universe._

    val compileTimeOnlyAnnotation = typeOf[compileTimeOnly]

    val compileTimeOnlyFound =
      compileLiteralString(c)(expr) exists {
        case tree: RefTree =>
          tree.symbol.annotations exists { _.tree.tpe <:< compileTimeOnlyAnnotation }
        case _ =>
          false
      }

    q"$compileTimeOnlyFound"
  }

  def containsValueOfType[T, U]: Boolean =
    macro containsValueOfTypeImpl[T, U]

  def containsValueOfTypeImpl[T: c.WeakTypeTag, U: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val T = weakTypeOf[T]
    val U = weakTypeOf[U]

    q"${T.decls exists { _.info.finalResultType <:< U } }"
  }

  private def compileLiteralString(c: whitebox.Context)(tree: c.Tree) = {
    def reportException(pos: Position, msg: String) = pos match {
      case pos: c.universe.Position @unchecked => c.abort(pos, msg)
      case _ => c.abort(c.enclosingPosition, msg)
    }

    import c.universe._

    tree match {
      case Literal(Constant(expr: String)) =>
        try c.typecheck(c.parse(expr))
        catch {
          case e: ParseException => reportException(e.pos, e.msg)
          case e: TypecheckException => reportException(e.pos, e.msg)
        }

      case _ =>
        c.abort(c.enclosingPosition, "string literal expected")
    }
  }

  private def failTest(c: whitebox.Context)(message: String) = {
    import c.universe._

    q"""throw new ${termNames.ROOTPKG}.org.scalatest.exceptions.TestFailedException(
      _ => ${termNames.ROOTPKG}.scala.Some($message),
      ${termNames.ROOTPKG}.scala.None,
      ${termNames.ROOTPKG}.scala.Left(${termNames.ROOTPKG}.org.scalactic.source.Position.here),
      ${termNames.ROOTPKG}.scala.None,
      ${termNames.ROOTPKG}.scala.Vector.empty)"""
  }
}
