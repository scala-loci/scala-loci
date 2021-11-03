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
            case tree if tree.tpe real_<:< types.remoteGateway =>

              val (expr, tpts, exprss, isNew) = tree match {
                case q"new $expr[..$tpts](...$exprss)" => (expr, tpts, exprss, true)
                case q"$expr[..$tpts](...$exprss)" => (expr, tpts, exprss, false)
                case tree => c.abort(tree.pos, s"Unexpected remote gateway: $tree")
              }

              count += 1

              val index = checkForConnection(exprss, tree.pos)

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
                if (isNew) {
                  transform(q"new $expr[..${tpts map createTypeTree}](${trees.remoteGateway})(..$exprs)")
                } else {
                  transform(q"$expr[..${tpts map createTypeTree}](${trees.remoteGateway})(..$exprs)")
                }
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

  def peerSignature(tpe: Type, pos: Position): Select = {
    object transformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case This(module.className) =>
          Ident(module.self)
        case _ =>
          super.transform(tree)
      }
    }

    val name = TermName(s"$$loci$$peer$$sig$$${tpe.typeSymbol.name}")
    val Select(qualifier, _) = createTypeTree(tpe, pos): @unchecked
    Select(transformer transform qualifier, name)
  }

  private def checkForConnection(exprss: List[List[Tree]], pos: Position): Int = {
    if (exprss.size != 2 || exprss.head.size != 1)
      c.abort(pos, "Invalid gateway accessor: " +
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
      c.abort(pos, "Invalid gateway accessor: " +
        "Exactly one connection value required")

    result
  }

  def createMultipleGateway(remoteType: Type, pos: Position): Tree = {
    //GatewayConnection is replaced in gateway:access, but its typeArgs are reused
    val gatewayConnectionTypeArgs = List(remoteType, types.transmitterMultiple)
    val gatewayConnection: Tree = createTypeTree(TypeOps(types.gatewayConnection).mapArgs(_ => gatewayConnectionTypeArgs), pos)
    val connection = internal.setType(
      q"new $gatewayConnection(${peerSignature(remoteType, pos)}, $$loci$$sys)",
      gatewayConnection.tpe
    )

    val defaultMultipleGatewayTypeArgs = List(remoteType)
    val defaultMultipleGateway: Tree = createTypeTree(TypeOps(types.defaultMultipleGateway).mapArgs(_ => defaultMultipleGatewayTypeArgs), pos)
    internal.setType(
      q"new $defaultMultipleGateway(${names.root}.loci.`package`.remote[$remoteType])($connection)",
      defaultMultipleGateway.tpe
    )
  }
}
