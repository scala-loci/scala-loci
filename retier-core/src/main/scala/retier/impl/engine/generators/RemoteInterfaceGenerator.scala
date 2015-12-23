package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait RemoteInterfaceGenerator { this: Generation =>
  val c: Context
  import c.universe._

  val generateRemoteInterfaces = UniformAggregation[
    PeerDefinition with PlacedStatement] {
      aggregator =>

    echo(verbose = true, s" Generating remote interface for placed expressions")

    val peerSymbols = aggregator.all[PeerDefinition] map { _.peerSymbol }

    val stats = aggregator.all[PlacedStatement] map { stat =>
      stat.copy(expr =
        new RemoteInterfaceGenerator(peerSymbols) transform stat.expr)
    }

    echo(verbose = true,
      s"  [${stats.size} placed statements generated, existing replaced]")

    aggregator replace stats
  }

  private class RemoteInterfaceGenerator(peerSymbols: List[TypeSymbol])
      extends Transformer {
    override def transform(tree: Tree) = tree match {
      case q"$_[..$tpts](...$exprss)"
          if symbols.connection contains tree.symbol =>
        val Seq(remote, _, _) = tpts
        val remotePeerTypeTag = peerTypeTagTree(
          remote.typeTree(abortOnFailure = true), remote.tpe, peerSymbols)

        val ExistentialType(_, TypeRef(pre, sym, _)) = types.peerTypeTag
        val remotePeerType = internal typeRef (pre, sym, List(remote.tpe))

        val remoteTypeTag = markRetierSynthetic(
          internal setType (remotePeerTypeTag, remotePeerType),
          tree.pos)

        import names._

        val createConnection = tree.symbol match {
          case symbols.multipleConnection => createMultipleRemoteConnection
          case symbols.optionalConnection => createOptionalRemoteConnection
          case symbols.singleConnection => createSingleRemoteConnection
        }

        q"$system.$createConnection($remoteTypeTag)"

      case _ if tree.tpe != null &&
                tree.tpe <:< types.connectionInterface &&
                (types.bottom forall { tree.tpe <:!< _ }) =>
        c.abort(tree.pos, "unexpected value of type RemoteConnectionInterface")

      case _ =>
        super.transform(tree)
    }
  }
}
