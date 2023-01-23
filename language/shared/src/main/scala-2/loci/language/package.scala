package loci
package language

import loci.embedding._

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros


@compileTimeOnly("enable macro paradise to use multitier code")
final class multitier extends StaticAnnotation {
  def this(accessorGeneration: AccessorGeneration) = this()
  def macroTransform(annottees: Any*): Any = macro impl.Multitier.annotation
}

object multitier {
  def start[P, Inst[P] <: Instance[P]](instance: Inst[P]): Runtime[P] =
    macro impl.Instance.start

  @compileTimeOnly("method can only be invoked in multitier code")
  def running: Boolean = erased

  @compileTimeOnly("method can only be invoked in multitier code")
  def terminate(): Unit = erased
}
