package loci
package transmitter

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

class DummyImplicit

object DummyImplicit {
  implicit def dummy: DummyImplicit = new DummyImplicit
  implicit def noDummy: DummyImplicit = macro NoDummyImplicit.skip
}

object NoDummyImplicit {
  def skip(c: whitebox.Context): c.Tree =
    c.abort(c.enclosingPosition, "Skipping dummy macro")
}
