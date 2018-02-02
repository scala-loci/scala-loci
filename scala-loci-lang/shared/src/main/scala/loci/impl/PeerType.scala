package loci
package impl

import scala.annotation.compileTimeOnly
import scala.util.Try
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.reflect.macros.whitebox.Context

private final case class PeerTypeImpl(name: String, bases: List[PeerType])
  extends PeerTypeImplBase

private final case class PeerTypeTagImpl[P](peerType: PeerTypeImpl)
  extends PeerTypeTagImplBase[P]

object PeerTypeTag {
  def create[P](name: String, bases: List[PeerType]): PeerTypeTag[P] =
    PeerTypeTagImpl(PeerTypeImpl(name, bases))

  @compileTimeOnly(
    "peer type tag should have been eliminated " +
    "while expanding `multitier` environment")
  def dummy[P]: PeerTypeTag[P] = ???

  def impl[P: c.WeakTypeTag]
      (c: Context)(ev: c.Expr[ClassTag[P]]): c.Expr[PeerTypeTag[P]] = {
    import c.universe._

    if (!c.hasErrors) {
      val enclosingMultitierMacro = c.enclosingMacros exists { c =>
        import c.universe._
        val multitierSymbol = c.mirror.staticClass("_root_.loci.multitier")

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
    }

    c.Expr[PeerTypeTag[P]](
      q"_root_.loci.impl.PeerTypeTag.dummy[${weakTypeOf[P]}]")
  }
}

object PeerType {
  private val escape = """[\{\}\\,]""".r
  private val unescape = """\\.""".r

  def serialize(peerType: PeerType): String = {
    val name = escape replaceAllIn (
      peerType.name,
      matching => Regex quoteReplacement ("""\""" + matching.matched))

    if (peerType.bases.isEmpty)
      name
    else
      name + (peerType.bases map serialize mkString ("{", ",", "}"))
  }

  def deserialize(string: String): Try[PeerType] = Try {
    var pos = 0
    var name = 0
    var depth = 0
    var escaped = false
    val bases = ListBuffer.empty[PeerType]

    for ((ch, index) <- string.zipWithIndex)
      if (ch == '\\')
        escaped = true
      else if (escaped)
        escaped = false
      else if (depth == 0 && name != 0)
        throwIllegalArgumentException
      else if (ch == '{') {
        if (depth == 0) {
          name = index
          pos = index + 1
        }
        depth += 1
      }
      else if (ch == '}') {
        if (depth == 1)
          bases += deserialize(string substring (pos, index)).get
        depth -= 1
        if (depth < 0)
          throwIllegalArgumentException
      }
      else if (ch == ',') {
        if (depth == 1) {
          bases += deserialize(string substring (pos, index)).get
          pos = index + 1
        }
        if (depth == 0)
          throwIllegalArgumentException
      }

    if (depth != 0)
      throwIllegalArgumentException

    if (name == 0)
      name = string.size

    PeerTypeImpl(
      unescape replaceAllIn (
        string substring (0, name),
        Regex quoteReplacement _.matched(1).toString),
      bases.toList)
  }

  private def throwIllegalArgumentException =
    throw new IllegalArgumentException("Malformed peer type representation")
}
