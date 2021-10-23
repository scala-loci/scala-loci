package loci.language.impl.components

import loci.language.impl.Component
import loci.language.impl.Engine
import loci.language.impl.Phase

import scala.reflect.macros.blackbox

object RemoteValueReference extends Component.Factory[RemoteValueReference](
  requires = Seq(Commons, Initialization, Peers, ModuleInfo)
) {
  override def asInstance[C <: blackbox.Context]: PartialFunction[Component[C], RemoteValueReference[C]] = {
    case c: RemoteValueReference[C] => c
  }

  override def apply[C <: blackbox.Context](engine: Engine[C]): RemoteValueReference[C] = new RemoteValueReference[C](engine)
}

class RemoteValueReference[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {

  override val phases: Seq[Phase] = Seq(
    Phase(
      "valueref:uuid",
      introduceUniquePeerIds,
      after = Set("init:inst"),
      before = Set("*", "unionpeer:group")
    )
  )

  private val commons = engine.require(Commons)
  private val initialization = engine.require(Initialization)
  private val peers = engine.require(Peers)
  private val moduleInfo = engine.require(ModuleInfo)

  import commons._
  import initialization._
  import peers._
  import moduleInfo._
  import engine.c
  import engine.c.universe._

  /**
   * This phase is executed before "unionpeer:group".
   * It places a `val $loci$peer$unique$id` on the union of all peer types in the module. The value is generated
   * for each instance and should be unique for each instance. It can be used to identify an instance.
   */
  def introduceUniquePeerIds(records: List[Any]): List[Any] = {

    /**
     * Create a `val` definition with type `String on (P1 | P2 | ...)` with `P1, P2, ...` being the module's peer types.
     * @param peers all peers of the module
     * @return a ValDef if the peers are non-empty
     */
    def createPlacedUniquePeerIdValDef(peers: Seq[Symbol]): Option[ValDef] = {
      val peerTypes = peers.map(_.asType.toType)
      val combinedPeerType = peerTypes match {
        case Seq() => None
        case Seq(singlePeer) => Some(singlePeer)
        case multiplePeers => Some(createUnionPeerType(multiplePeers))
      }
      combinedPeerType.map { combinedPeerType =>
        val onType = types.on mapArgs { _ => List(types.uniquePeerId, combinedPeerType) }
        val onTypeTree = createTypeTree(onType, NoPosition)

        val syntheticName = TermName("$loci$synthetic")
        val placedName = TermName("placed")
        val placedSymbol = internal.newMethodSymbol(symbols.Placed, placedName, flags = Flag.SYNTHETIC)
        val placed = internal.setSymbol(q"$syntheticName.$placedName", placedSymbol)
        val placedContextTypeTree: Tree = createTypeTree(types.context.mapArgs(_ => List(combinedPeerType)), NoPosition)
        val placedContextParam = ValDef(Modifiers(Flag.IMPLICIT | Flag.PARAM), TermName("$bang"), placedContextTypeTree, EmptyTree)

        // Setting a reasonable methodType as info is necessary for transforming the union peer types during "unionpeer:group"
        internal.setInfo(placedSymbol, internal.methodType(
          List(internal.setInfo(
            internal.newTermSymbol(placedSymbol, TermName("$placed$param"), NoPosition, Flag.PARAM),
            types.function mapArgs { _ => List(placedContextTypeTree.tpe, types.uniquePeerId) }
          )),
          onType
        ))

        val name = TermName(s"$$loci$$peer$$unique$$id$$${module.className.toString}")
        val symbol = internal.newTermSymbol(module.classSymbol, name, NoPosition, Flag.SYNTHETIC | Flag.PRIVATE | Flag.LOCAL)
        internal.setInfo(symbol, onType)

        val generateId = internal.setType(
          q"val id: ${types.uniquePeerId} = ${trees.generateUniquePeerId}; ${names.root}.scala.Predef.println(id); id",
          types.uniquePeerId
        )

        val body = internal.setType(
          q"$placed((..$placedContextParam) => $generateId)",
          onType
        )

        val definition = q"${Flag.SYNTHETIC} val $name: $onTypeTree = $body"
        internal.setSymbol(definition, symbol)
        internal.setType(definition, onType)

        definition
      }
    }

    records process {
      case Initialized(tree) =>
        //val peers = getModulePeers(tree.impl.body)
        val peers = tree.impl.body.collect {
          case tree: TypeDef if validatePeerType(tree.symbol, tree.pos).isDefined => tree.symbol
        }
        val uniquePeerId = createPlacedUniquePeerIdValDef(peers)

        val result = Initialized(ImplDefOps(tree).map { (mods, parents, self, body) =>
          (mods, parents, self, body ++ uniquePeerId)
        })

        uniquePeerId.foreach { _ =>
          logging.debug(s" Created placed unique peer id on peers ${peers.map(_.name.toString).mkString(", ")}")
        }

        result
    }
  }

  private def createUnionPeerType(peerTypes: Seq[Type]): Type = {
    peerTypes match {
      case Seq(singlePeer) => singlePeer
      case head :: tail if tail.nonEmpty =>
        types.union mapArgs { _ => List(head, createUnionPeerType(tail)) }
    }
  }

}
