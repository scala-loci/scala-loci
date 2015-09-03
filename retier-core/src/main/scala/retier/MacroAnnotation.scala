package retier

import scala.annotation.compileTimeOnly
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

@compileTimeOnly("enable macro paradise to expand macro annotations")
class multitier extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro impl.engine.multitier.impl
}
