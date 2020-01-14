package loci
package language
package impl
package components

import scala.reflect.macros.blackbox

object GatewayAccess extends Component.Factory[GatewayAccess](
    requires = Seq(Commons, ModuleInfo, Values)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new GatewayAccess(engine)
  def asInstance[C <: blackbox.Context] = { case c: GatewayAccess[C] => c }
}

class GatewayAccess[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq(
    Phase("gateway:access", processGatewayAccess, after = Set("values:collect"), before = Set("impls:lift")))

  val commons = engine.require(Commons)
  val moduleInfo = engine.require(ModuleInfo)
  val values = engine.require(Values)

  import engine._
  import engine.c.universe._
  import commons._
  import moduleInfo._
  import values._

  def processGatewayAccess(records: List[Any]): List[Any] = {
    var count = 0
    val result = records process {
      case record @ PlacedValue(_, _, _, _) =>
        object transformer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case tree @ q"$expr[..$tpts](...$exprss)" if tree.tpe real_<:< types.remoteGateway =>
              count += 1

              val index = checkForConnection(tree)

              val pos = exprss.head.head.pos
              val args = exprss(1)(index).tpe.underlying.typeArgs

              exprss.head.head match {
                case q"$_[..$tpts](...$_)" if tpts.isEmpty =>
                  c.abort(pos, "No remote peer specified")
                case _ =>
              }

              val remoteGateway = createTypeTree(
                types.gatewayConnection mapArgs { _ => args },
                pos)

              val exprs = exprss(1).updated(
                index,
                q"new $remoteGateway(${peerSignature(args.head, pos)}, $$loci$$sys)")

              atPos(pos) {
                transform(q"$expr[..${tpts map createTypeTree}](${trees.remoteGateway})(..$exprs)")
              }

            case _ =>
              super.transform(tree)
          }
        }

        record.copy(tree = transformer transform record.tree)
    }

    logging.debug(s" Processed $count remote connection state ${if (count == 1) "query" else "queries"}")

    result
  }

  private def peerSignature(tpe: Type, pos: Position) = {
    object transformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case This(module.className) =>
          Ident(module.self)
        case _ =>
          super.transform(tree)
      }
    }

    val name = TermName(s"$$loci$$peer$$sig$$${tpe.typeSymbol.name}")
    val Select(qualifier, _) = createTypeTree(tpe, pos)
    Select(transformer transform qualifier, name)
  }

  private def checkForConnection(tree: Tree): Int = {
    val q"$_[..$_](...$exprss)" = tree

    if (exprss.size != 2 || exprss.head.size != 1)
      c.abort(tree.pos, "Invalid gateway accessor: " +
        "Implicit conversion with implicit argument list required")

    var index = 0
    var count = 0
    var result = -1

    exprss(1) foreach { expr =>
      if (expr.tpe real_<:< types.connection) {
        result = index
        count += 1
      }

      index += 1
    }

    if (count != 1)
      c.abort(tree.pos, "Invalid gateway accessor: " +
        "Exactly one connection value required")

    result
  }
}
