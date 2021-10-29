package loci.language.impl.components

import loci.language.impl.Component
import loci.language.impl.Engine
import loci.language.impl.Phase

import scala.collection.mutable
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
      before = Set("*", "values:collect")
    ),
    Phase(
      "valueref:creation",
      processValueRefCreations,
      after = Set("valueref:uuid"),
      before = Set("*", "values:collect")
    )
  )

  private val commons = engine.require(Commons)
  private val initialization = engine.require(Initialization)
  private val moduleInfo = engine.require(ModuleInfo)

  import commons._
  import initialization._
  import moduleInfo._
  import engine.c.universe._

  private val uniquePeerIdName = TermName("$loci$peer$unique$id")

  /**
   * This phase is executed before "values:collect".
   * It adds a `val $loci$peer$unique$id` as a module value. The value is generated
   * for each instance and should be unique for each instance. It can be used to identify an instance.
   */
  def introduceUniquePeerIds(records: List[Any]): List[Any] = {

    /**
     * Create a `val $loci$peer$unique$id: UUID = UniquePeerId.generate()`
     */
    def createUniquePeerIdValDef: ValDef = {
      val uniquePeerIdTypeTree: Tree = createTypeTree(types.uniquePeerId, NoPosition)

      val symbol = internal.newTermSymbol(module.classSymbol, uniquePeerIdName, NoPosition, Flag.SYNTHETIC | Flag.PRIVATE | Flag.LOCAL)
      internal.setInfo(symbol, types.uniquePeerId)

      val generateId = internal.setType(
        q"val id: ${types.uniquePeerId} = ${trees.generateUniquePeerId}; ${names.root}.scala.Predef.println(id); id",
        types.uniquePeerId
      )

      val definition: ValDef = q"${Flag.SYNTHETIC} val $uniquePeerIdName: $uniquePeerIdTypeTree = $generateId"
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

        logging.debug(s" Created placed unique peer id")

        result
    }
  }

  /**
   * This phase is executed before "values:collect" but after "valueref:uuid".
   * It processes occurrences of the `ValueRefCreator` implicit class and transforms its implicit parameters, which
   * are filled with compile-time dummies. The context parameter is set to null, the peerId is replaced with the
   * unique peer id value introduced in "valueref:uuid", and the cache is replaced with a new cache `ValDef`.
   * For each value type there is one cache that is added as a module value.
   */
  def processValueRefCreations(records: List[Any]): List[Any] = {

    case class SyntheticCacheDefinition(name: TermName, definition: ValDef, valueType: Type)

    val syntheticCacheDefinitions = mutable.ListBuffer.empty[SyntheticCacheDefinition]

    def createCacheName(valueType: Type): TermName = {
      val valueTypeName = valueType.typeSymbol.fullName.replace('.', '$')
      TermName(s"$$loci$$value$$cache$$$valueTypeName")
    }

    def getOrCreateSyntheticCacheDefintion(valueType: Type): SyntheticCacheDefinition = {
      syntheticCacheDefinitions.find(_.valueType == valueType).getOrElse {
        val cacheType = types.peerValueCache mapArgs { _ => List(valueType) }
        val cacheTypeTree: Tree = createTypeTree(cacheType, NoPosition)
        val cacheImplType = types.peerValueMapCache mapArgs { _ => List(valueType) }
        val cacheImplTypeTree: Tree = createTypeTree(cacheImplType, NoPosition)

        val name = createCacheName(valueType)

        val symbol = internal.newTermSymbol(module.classSymbol, name, NoPosition, Flag.SYNTHETIC | Flag.PRIVATE | Flag.LOCAL)
        internal.setInfo(symbol, cacheType)

        val newCache = internal.setType(q"new $cacheImplTypeTree", cacheType)

        val definition: ValDef = q"${Flag.SYNTHETIC} val $name: $cacheTypeTree = $newCache"
        internal.setSymbol(definition, symbol)
        internal.setType(definition, cacheType)

        val cacheDef = SyntheticCacheDefinition(name, definition, valueType)
        syntheticCacheDefinitions.append(cacheDef)
        cacheDef
      }
    }

    def replaceValueRefCreatorArgs(valueRefCreator: Tree): Tree = {
      val q"$creator[..$tpts](...$exprss)" = valueRefCreator: @unchecked
      val List(List(value), List(_, cache, _)) = exprss.asInstanceOf[List[List[Tree]]]: @unchecked
      val valueType = cache.tpe.typeArgs.head

      val peerId = Ident(uniquePeerIdName)
      val generatedCache = Ident(getOrCreateSyntheticCacheDefintion(valueType).name)
      val nullContext = q"null" // nulling the context ensures that it does not fail due to unexpected multitier construct in "values:validate"

      val replacedCreator = internal.setType(
        q"$creator[..$tpts]($value)($peerId, $generatedCache, $nullContext)",
        valueRefCreator.tpe
      )
      replacedCreator
    }

    records process {
      case Initialized(tree) =>
        object ValueRefCreatorTransformer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case tree if tree.tpe real_<:< types.valueRefCreator =>
              super.transform(replaceValueRefCreatorArgs(tree))
            case tree => super.transform(tree)
          }
        }

        val result = Initialized(ImplDefOps(tree).map { (mods, parents, self, body) =>
          (mods, parents, self, ValueRefCreatorTransformer.transformTrees(body) ++ syntheticCacheDefinitions.map(_.definition))
        })

        syntheticCacheDefinitions.foreach {
          case SyntheticCacheDefinition(name, _, _) => logging.debug(s" Created peer value cache $name")
        }

        result
    }

  }

}
