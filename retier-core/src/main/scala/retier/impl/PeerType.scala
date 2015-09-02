package retier
package impl

import scala.annotation.compileTimeOnly
import scala.reflect.ClassTag
import scala.reflect.macros.whitebox.Context

private final case class PeerTypeImpl(name: String, bases: List[PeerType])
    extends PeerTypeImplBase

private final case class PeerTypeTagImpl[P](peerType: PeerTypeImpl)
    extends PeerTypeTagImplBase[P]

object PeerTypeTag {
  def create[P](name: String, bases: List[PeerType]): PeerTypeTag[P] =
    PeerTypeTagImpl(PeerTypeImpl(name, bases))

  @annotation.compileTimeOnly(
    "peer type tag should have been eliminated " +
    "while expanding `multitier` environment")
  def dummy[P]: PeerTypeTag[P] = ???

  def impl[P: c.WeakTypeTag]
      (c: Context)(ev: c.Expr[ClassTag[P]]): c.Expr[PeerTypeTag[P]] = {
    import c.universe._

    val enclosingMultitierMacro = c.enclosingMacros exists { c =>
      import c.universe._
      val multitierSymbol = c.mirror.staticClass("_root_.retier.multitier")

      c.macroApplication match {
        case q"new $expr[..$tpts](...$exprss).macroTransform[..$_](...$_)" =>
          val annotation = q"new $expr[..$tpts](...$exprss)"
          (c typecheck (annotation, silent = true)) match {
            case q"new $expr[..$_](...$_)" =>
              expr.symbol == multitierSymbol
            case _ =>
              false
          }
        case _ =>
          false
      }
    }

    if (!enclosingMultitierMacro)
      c.abort(c.enclosingPosition,
        "Removing macro from the list of implicit candidates " +
        "since there is no enclosing `multitier` macro being expanded")

    c.Expr[PeerTypeTag[P]](
      q"_root_.retier.impl.PeerTypeTag.dummy[${weakTypeOf[P]}]")
  }
}
