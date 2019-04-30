package loci.dev
package language
package impl
package components

import scala.reflect.macros.blackbox

object Subjectivity extends Component.Factory[Subjectivity](
    requires = Seq(Commons, Values)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new Subjectivity(engine)
  def asInstance[C <: blackbox.Context] = { case c: Subjectivity[C] => c }
}

class Subjectivity[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq(
    Phase("local:sbj", processLocalSubjectiveAccess, after = Set("values:validate"), before = Set("impls:lift")))

  val commons = engine.require(Commons)
  val values = engine.require(Values)

  import engine.c.universe._
  import commons._
  import values._

  def processLocalSubjectiveAccess(records: List[Any]): List[Any] =
    records process {
      case record @ PlacedValue(_, _, _, _) =>
        object transformer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case q"$expr.$_[..$_]($_[..$_](...$exprss))"
                if tree.nonEmpty &&
                   tree.symbol == symbols.to =>
              decomposePlacementType(expr.tpe.widen, EmptyTree, expr.symbol, expr.pos, moduleDefinition = false) match {
                case Placed(_, _, _, Modality.Subjective(_)) =>
                  val value = transform(expr)
                  val remote = transform(exprss.head.head)

                  if (expr.symbol.isTerm && expr.symbol.asTerm.isStable)
                    q"$$loci$$sys.subjectiveValue($value, $remote)"
                  else
                    q"$value($remote)"

                case _ =>
                  transform(expr)
              }

            case _ =>
              super.transform(tree)
          }
        }

        record.copy(tree = transformer transform record.tree)
    }
}
