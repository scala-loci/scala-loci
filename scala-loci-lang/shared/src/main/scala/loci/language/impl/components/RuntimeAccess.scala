package loci
package language
package impl
package components

import scala.reflect.macros.blackbox

object RuntimeAccess extends Component.Factory[RuntimeAccess](
    requires = Seq(Commons, Values)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new RuntimeAccess(engine)
  def asInstance[C <: blackbox.Context] = { case c: RuntimeAccess[C] => c }
}

class RuntimeAccess[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq(
    Phase("runtime:access", processRuntimeAccess, after = Set("values:collect"), before = Set("impls:lift")))

  val commons = engine.require(Commons)
  val values = engine.require(Values)

  import engine.c.universe._
  import commons._
  import values._

  def processRuntimeAccess(records: List[Any]): List[Any] = {
    var count = 0
    val result = records process {
      case record @ PlacedValue(_, _, _, _) =>
        object transformer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case q"$_.$name[..$tpts](...$exprss)"
                if tree.symbol.owner == symbols.multitier &&
                  (name == names.running || name == names.terminate) =>
              count += 1
              q"$$loci$$sys.$name[..$tpts](...$exprss)"

            case _ =>
              super.transform(tree)
          }
        }

        record.copy(tree = transformer transform record.tree)
    }

    logging.debug(s" Processed $count multitier runtime ${if (count == 1) "query" else "queries"}")

    result
  }
}
