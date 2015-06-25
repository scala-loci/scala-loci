package retier

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

class multitier extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro impl.engine.multitier.impl
}
