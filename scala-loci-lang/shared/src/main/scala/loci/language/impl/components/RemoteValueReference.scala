package loci.language.impl.components

import loci.language.impl.Component
import loci.language.impl.Engine
import loci.language.impl.Phase

import scala.reflect.macros.blackbox

object RemoteValueReference extends Component.Factory[RemoteValueReference](
  requires = Seq(Commons, Initialization, ModuleInfo)
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
  private val moduleInfo = engine.require(ModuleInfo)

  import commons._
  import initialization._
  import moduleInfo._
  import engine.c.universe._

  /**
   * This phase is executed before "unionpeer:group".
   * It adds a `val $loci$peer$unique$id` as a module value. The value is generated
   * for each instance and should be unique for each instance. It can be used to identify an instance.
   */
  def introduceUniquePeerIds(records: List[Any]): List[Any] = {

    /**
     * Create a `val $loci$peer$unique$id: UUID = UniquePeerId.generate()`
     */
    def createUniquePeerIdValDef: ValDef = {
      val uniquePeerIdTypeTree: Tree = createTypeTree(types.uniquePeerId, NoPosition)

      val name = TermName("$loci$peer$unique$id")
      val symbol = internal.newTermSymbol(module.classSymbol, name, NoPosition, Flag.SYNTHETIC | Flag.PRIVATE | Flag.LOCAL)
      internal.setInfo(symbol, types.uniquePeerId)

      val generateId = internal.setType(
        q"val id: ${types.uniquePeerId} = ${trees.generateUniquePeerId}; ${names.root}.scala.Predef.println(id); id",
        types.uniquePeerId
      )

      val definition: ValDef = q"${Flag.SYNTHETIC} val $name: $uniquePeerIdTypeTree = $generateId"
      internal.setSymbol(definition, symbol)
      internal.setType(definition, types.uniquePeerId)

      definition
    }

    records process {
      case Initialized(tree) =>
        val uniquePeerId = createUniquePeerIdValDef

        val result = Initialized(ImplDefOps(tree).map { (mods, parents, self, body) =>
          (mods, parents, self, body :+ uniquePeerId)
        })

        uniquePeerId.foreach { _ =>
          logging.debug(s" Created placed unique peer id")
        }

        result
    }
  }

}
