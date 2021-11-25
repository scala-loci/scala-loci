package loci.language.impl.components

import loci.language.impl.Component
import loci.language.impl.Engine
import loci.language.impl.Phase

import scala.collection.mutable
import scala.reflect.macros.blackbox

object RemoteValueReference extends Component.Factory[RemoteValueReference](
  requires = Seq(Commons, Initialization, ModuleInfo, GatewayAccess)
) {
  override def asInstance[C <: blackbox.Context]: PartialFunction[Component[C], RemoteValueReference[C]] = {
    case c: RemoteValueReference[C] => c
  }

  override def apply[C <: blackbox.Context](engine: Engine[C]): RemoteValueReference[C] = new RemoteValueReference[C](engine)
}

class RemoteValueReference[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {

  override val phases: Seq[Phase] = Seq(
    Phase(
      "valueref:creation",
      processValueRefCreations,
      after = Set("init:inst"),
      before = Set("*", "values:collect")
    ),
    Phase(
      "valueref:access",
      processValueRefAccesses,
      after = Set("valueref:creation"),
      before = Set("*", "remote:block")
    )
  )

  private val commons = engine.require(Commons)
  private val initialization = engine.require(Initialization)
  private val moduleInfo = engine.require(ModuleInfo)
  private val gatewayAccess = engine.require(GatewayAccess)

  import commons._
  import initialization._
  import moduleInfo._
  import gatewayAccess._
  import engine.c.universe._

  /**
   * This phase is executed before "values:collect".
   * It processes occurrences of the `ValueRefCreator` implicit class and transforms its implicit parameters, which
   * are filled with compile-time dummies. The context parameter is set to null, the peerId is replaced with
   * `$loci$sys.peerId`, and the cache is replaced with `$loci$sys.peerValueCache`.
   */
  def processValueRefCreations(records: List[Any]): List[Any] = {
    var count = 0

    def replaceValueRefCreatorArgs(valueRefCreator: Tree): Tree = {
      val q"$creator[..$tpts](...$exprss)" = valueRefCreator: @unchecked
      val List(List(value), List(_, _, _, _)) = exprss.asInstanceOf[List[List[Tree]]]: @unchecked
      val peerType = tpts.last.tpe

      val peerId = q"$$loci$$sys.peerId"
      val peerValueCache = q"$$loci$$sys.peerValueCache"
      val signature = q"$$loci$$sys.instanceSignature"
      val nullContext = q"null" // nulling the context ensures that it does not fail due to unexpected multitier construct in "values:validate"

      val replacedCreator = internal.setType(
        q"$creator[..$tpts]($value)($peerId, $peerValueCache, $signature, $nullContext)",
        valueRefCreator.tpe
      )
      replacedCreator
    }

    object ValueRefCreatorTransformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case tree if tree.tpe real_<:< types.valueRefCreator =>
          count += 1
          super.transform(replaceValueRefCreatorArgs(tree))
        case tree => super.transform(tree)
      }
    }

    records process {
      case Initialized(tree) =>
        val result = Initialized(ImplDefOps(tree).map { (mods, parents, self, body) =>
          (mods, parents, self, ValueRefCreatorTransformer.transformTrees(body))
        })

        logging.debug(s" Processed $count value reference creations")

        result
    }

  }

  /**
   * This phase is executed before "remote:block".
   * It processes occurrences of the `ValueRefAccessor` implicit class and transforms its implicit parameters, which
   * are filled with compile-time dummies. The context parameter is set to null, the executionContext is not changed.
   * For the gateway we create a `DefaultMultipleGateway` that will be further processed in "gateway:access".
   * For the remotePeerIdAccess and cacheValueAccess we create a `BasicSingleAccessor` each, using the parameters from
   * the dummy implicit. The `BasicSingleAccessor` contains a remote block as value, which is further processed in
   * "remote:block".
   */
  def processValueRefAccesses(records: List[Any]): List[Any] = {
    var count = 0

    def replaceValueRefAccessorArgs(valueRefAccessor: Tree): Tree = {
      val q"$accessor[..$tpts](...$exprss)" = valueRefAccessor: @unchecked
      val List(
        List(value),
        List(gateway, remotePeerIdAccess, cacheValueAccess, _, executionContext)
      ) = exprss.asInstanceOf[List[List[Tree]]]: @unchecked

      val nullContext = q"null" // nulling the context ensures that it does not fail due to unexpected multitier construct in "values:validate"
      val actualGateway = createMultipleGateway(gateway.tpe.typeArgs.head, gateway.pos)
      val actualRemotePeerIdAccess = generatePeerIdAccess(remotePeerIdAccess)
      val actualCacheValueAccess = generateCacheValueAccess(cacheValueAccess)

      internal.setType(
        q"$accessor[..$tpts]($value)($actualGateway, $actualRemotePeerIdAccess, $actualCacheValueAccess, $nullContext, $executionContext)",
        valueRefAccessor.tpe
      )
    }

    /**
     * Uses transmission, placedClean, and canonicalPlacedTypeAlias that are given as implicit parameters in the
     * dummy access ([[loci.valueref.CompileTimeDummyImplicits.dummyRemotePeerIdAccess]]. Returns a function that maps
     * a remote to an access of the unique peer id on that particular remote.
     */
    def generatePeerIdAccess(dummyAccess: Tree): Tree = {
      val q"$_[..$_]($transmission, $placedClean, $canonicalPlacedTypeAlias)" = dummyAccess: @unchecked
      val List(remoteType, basicSingleAccessorType) = dummyAccess.tpe.typeArgs: @unchecked

      val peerId = internal.setType(q"$$loci$$sys.peerId", types.uniquePeerId)

      val remote: Tree = createTypeTree(remoteType, dummyAccess.pos)
      val uuidPlacedRemote: Tree = createTypeTree(canonicalPlacedTypeAlias.tpe.typeArgs.last, dummyAccess.pos)

      val remoteParam = ValDef(Modifiers(Flag.PARAM), TermName("$remote"), remote, EmptyTree)
      val remoteIdent = internal.setType(Ident(remoteParam.name), remoteType)

      val placedContextTypeTree: Tree = createTypeTree(types.context.mapArgs(_ => List(remoteType.typeArgs.head)), dummyAccess.pos)
      val placedContextParam = internal.setPos(
        ValDef(Modifiers(Flag.IMPLICIT | Flag.PARAM), TermName("$bang"), placedContextTypeTree, EmptyTree),
        dummyAccess.pos
      )

      val remoteBlockBody = internal.setType(
        q"(..$placedContextParam) => $peerId",
        types.function.mapArgs(_ => List(placedContextTypeTree.tpe, peerId.tpe))
      )

      // symbols required for "remote:block" to lift the remote block and the position is required for "remote:block"
      // to create the lifted definition at this position, which is then further required by "remote:marshalling"
      val selectApply = internal.setSymbol(q"${names.root}.loci.`package`.on.apply", symbols.SelectApply)
      val run = atPos(dummyAccess.pos)(q"$selectApply[${remoteType.typeArgs.head}]($remoteIdent).run")
      val blockApply = internal.setSymbol(q"$run.apply", symbols.BlockApply)

      val accessedPeerId = internal.setType(
        q"$blockApply[${peerId.tpe}, ${peerId.tpe}, $uuidPlacedRemote]($remoteBlockBody)($placedClean, $canonicalPlacedTypeAlias)",
        uuidPlacedRemote.tpe
      )
      val appliedAccessor = internal.setType(
        q"${trees.basicSingleAccessor}[..${basicSingleAccessorType.typeArgs}]($accessedPeerId)($transmission)",
        basicSingleAccessorType
      )
      internal.setType(q"(..$remoteParam) => $appliedAccessor", dummyAccess.tpe)
    }

    /**
     * Uses transmission, placedClean, and canonicalPlacedTypeAlias that are given as implicit parameters in the
     * dummy access ([[loci.valueref.CompileTimeDummyImplicits.dummyCacheValueAccess]]. Returns a function that maps
     * a value id and a remote to an access of the peer value cache on that particular remote.
     *
     * The access happens in the form of a remote block with a single capture, namely the value id, which needs to be
     * passed to the accessed remote, so that it can fetch the respective value from its cache.
     */
    def generateCacheValueAccess(dummyAccess: Tree): Tree = {
      val q"$_[..$tpts]($transmission, $placedClean, $canonicalPlacedTypeAlias)" = dummyAccess: @unchecked
      val List(valueIdType, remoteType, basicSingleAccessorType) = dummyAccess.tpe.typeArgs: @unchecked

      val cacheValueType = tpts.head.tpe
      val cacheValueOptionType = types.option.mapArgs(_ => List(cacheValueType))
      val cache = q"$$loci$$sys.peerValueCache"

      val valueId: Tree = createTypeTree(valueIdType, dummyAccess.pos)
      val remote: Tree = createTypeTree(remoteType, dummyAccess.pos)
      val valuePlacedRemote: Tree = createTypeTree(canonicalPlacedTypeAlias.tpe.typeArgs.last, dummyAccess.pos)

      val valueIdParam = ValDef(Modifiers(Flag.PARAM), TermName("$value$id"), valueId, EmptyTree)
      val valueIdIdent = internal.setType(Ident(valueIdParam.name), valueIdType)
      val remoteParam = ValDef(Modifiers(Flag.PARAM), TermName("$remote"), remote, EmptyTree)
      val remoteIdent = internal.setType(Ident(remoteParam.name), remoteType)

      val placedContextTypeTree: Tree = createTypeTree(
        types.context.mapArgs(_ => List(remoteType.typeArgs.head)),
        dummyAccess.pos
      )
      val placedContextParam = internal.setPos(
        ValDef(Modifiers(Flag.IMPLICIT | Flag.PARAM), TermName("$bang"), placedContextTypeTree, EmptyTree),
        dummyAccess.pos
      )

      val remoteBlockBody = internal.setType(
        q"(..$placedContextParam) => $cache.getAs[$cacheValueType]($valueIdIdent)",
        types.function.mapArgs(_ => List(placedContextTypeTree.tpe, cacheValueOptionType))
      )

      // symbols required for "remote:block" to lift the remote block and the position is required for "remote:block"
      // to create the lifted definition at this position, which is then further required by "remote:marshalling"
      val selectApply = internal.setSymbol(q"${names.root}.loci.`package`.on.apply", symbols.SelectApply)
      val capture = atPos(dummyAccess.pos) {
        internal.setSymbol(
          q"$selectApply[${remoteType.typeArgs.head}]($remoteIdent).run.capture($valueIdIdent)",
          symbols.CaptureCapture
        )
      }
      val blockApply = internal.setSymbol(q"$capture.apply", symbols.BlockApply)

      val accessedValue = internal.setType(
        q"$blockApply[$cacheValueOptionType, $cacheValueOptionType, $valuePlacedRemote]($remoteBlockBody)($placedClean, $canonicalPlacedTypeAlias)",
        valuePlacedRemote.tpe
      )
      val appliedAccessor = internal.setType(
        q"${trees.basicSingleAccessor}[..${basicSingleAccessorType.typeArgs}]($accessedValue)($transmission)",
        basicSingleAccessorType
      )
      val access = internal.setType(q"(..${List(valueIdParam, remoteParam)}) => $appliedAccessor", dummyAccess.tpe)

      // symbols set in such a way that "remote:block" interprets the valueIdIdent as local identifier
      internal.setSymbol(
        access,
        internal.setInfo(
          internal.newTermSymbol(module.classSymbol, TermName("$anonfun"), dummyAccess.pos),
          dummyAccess.tpe
        )
      )
      internal.setSymbol(
        valueIdIdent,
        internal.setInfo(internal.newTermSymbol(access.symbol, valueIdParam.name, dummyAccess.pos), valueIdType)
      )

      access
    }

    object ValueRefAccessorTransformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case tree if tree.tpe real_<:< types.valueRefAccessor =>
          count += 1
          super.transform(replaceValueRefAccessorArgs(tree))
        case tree => super.transform(tree)
      }
    }

    records process {
      case Initialized(tree) =>
        val result = Initialized(ImplDefOps(tree).map { (mods, parents, self, body) =>
          (mods, parents, self, ValueRefAccessorTransformer.transformTrees(body))
        })

        logging.debug(s" Processed $count value reference accesses")

        result
    }
  }

}
