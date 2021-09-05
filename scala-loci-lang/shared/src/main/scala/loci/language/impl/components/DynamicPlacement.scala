package loci.language.impl.components

import loci.language.impl.Component
import loci.language.impl.Engine
import loci.language.impl.Phase

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

object DynamicPlacement extends Component.Factory[DynamicPlacement](
  requires = Seq(Commons, Initialization, GatewayAccess)
) {
  override def asInstance[C <: blackbox.Context]: PartialFunction[Component[C], DynamicPlacement[C]] = {
    case c: DynamicPlacement[C] => c
  }

  override def apply[C <: blackbox.Context](engine: Engine[C]): DynamicPlacement[C] = new DynamicPlacement[C](engine)
}

class DynamicPlacement[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {

  override val phases: Seq[Phase] = Seq(
    Phase(
      "dynamic:selection:normalize",
      normalizedSelectionRules,
      after = Set("init:inst"),
      before = Set("*", "remote:block")
    )
  )

  private val commons = engine.require(Commons)
  private val initialization = engine.require(Initialization)
  private val gatewayAccess = engine.require(GatewayAccess)

  import commons._
  import gatewayAccess._
  import initialization._
  import engine.c
  import engine.c.universe._


  /**
   * This phase is executed before "remote:block".
   * All arguments passed to `SelectAny.apply` in multitier code are normalized to type `Remote[_]`.
   */
  def normalizedSelectionRules(records: List[Any]): List[Any] = {
    records process {
      case Initialized(tree) =>

        var processedCount = 0
        var normalizedCount = 0

        /**
         * Selection rule of type `Seq[Remote[_]] => Remote[_]`?
         */
        def isRuleFunctionOnRemoteSeq(ruleType: Type): Boolean = {
          ruleType match {
            case tpe if (TypeOps(tpe) real_<:< types.function) && tpe.typeArgs.size == 2 =>
              val List(paramType, returnType) = tpe.typeArgs: @unchecked
              (TypeOps(paramType) real_<:< types.remoteSeq) && (TypeOps(returnType) real_<:< types.remote)
            case _ => false
          }
        }

        /**
         * Selection rule of type `Seq[Remote[_]] => Remote[_]`
         */
        def isParameterlessRule(ruleType: Type): Boolean = {
          TypeOps(ruleType) real_<:< types.remote
        }

        def isRuleFunctionOnRemoteSeqAndSelfReference(ruleType: Type): Boolean = {
          ruleType match {
            case tpe
              if (TypeOps(tpe) real_<:< types.biFunction)
                && tpe.typeArgs.size == 3 =>
              val List(firstParamType, secondParamType, returnType) = tpe.typeArgs: @unchecked
              (TypeOps(firstParamType) real_<:< types.remoteSeq) &&
                (TypeOps(secondParamType) real_<:< types.selfReference) &&
                (TypeOps(returnType) real_<:< types.remote)
            case _ => false
          }
        }

        def createRemoteGateway(ruleFunction: Tree, ruleOutputPeerType: Type): Tree = {
          //GatewayConnection is replaced in gateway:access, but its typeArgs are reused
          val gatewayConnectionTypeArgs = List(ruleOutputPeerType, types.transmitterMultiple)
          val gatewayConnection: Tree = createTypeTree(TypeOps(types.gatewayConnection).mapArgs(_ => gatewayConnectionTypeArgs), ruleFunction.pos)
          val connection = internal.setType(
            q"new $gatewayConnection(${peerSignature(ruleOutputPeerType, ruleFunction.pos)}, $$loci$$sys)",
            gatewayConnection.tpe
          )

          val defaultMultipleGatewayTypeArgs = List(ruleOutputPeerType)
          val defaultMultipleGateway: Tree = createTypeTree(TypeOps(types.defaultMultipleGateway).mapArgs(_ => defaultMultipleGatewayTypeArgs), ruleFunction.pos)
          internal.setType(
            q"new $defaultMultipleGateway(${names.root}.loci.`package`.remote[$ruleOutputPeerType])($connection)",
            defaultMultipleGateway.tpe
          )
        }

        /**
         * A rule argument of type `Seq[Remote[P]] => Remote[P]` is replaced by its application on
         * `remote[P].connected`, which yields return type `Remote[P]`
         *
         * @param ruleFunction       the argument initially passed as selection rule
         * @param ruleOutputPeerType the type of peer that is selected
         * @param ruleType           the type of the selection rule
         */
        def normalizeRuleFunctionOnRemoteSeq(ruleFunction: Tree, ruleOutputPeerType: Type, ruleType: Type): Tree = {
          val gateway = createRemoteGateway(ruleFunction, ruleOutputPeerType)
          val returnType = ruleType.typeArgs.last
          internal.setType(q"$ruleFunction($gateway.connected)", returnType)
        }

        /**
         * A rule argument of type `(Seq[Remote[P]], SelfReference[P]) => Remote[P]` is replaced by its application on
         * `remote[P].connected` and `new SelfReference[P]`, which yields return type `Remote[P]`
         *
         * @param ruleFunction       the argument initially passed as selection rule
         * @param ruleOutputPeerType the type of peer that is selected
         * @param ruleType           the type of the selection rule
         */
        def normalizeRuleFunctionOnRemoteSeqAndSelfReference(
          ruleFunction: Tree,
          ruleOutputPeerType: Type,
          ruleType: Type
        ): Tree = {
          val gateway = createRemoteGateway(ruleFunction, ruleOutputPeerType)

          val selfReferenceType = ruleType.typeArgs(1)
          val selfReferencePeerType = selfReferenceType.typeArgs.head
          val selfReference = internal.setType(
            q"new ${names.root}.loci.SelfReference[$selfReferencePeerType]",
            selfReferenceType
          )

          val returnType = ruleType.typeArgs.last
          internal.setType(q"$ruleFunction($gateway.connected, $selfReference)", returnType)
        }

        /**
         * Simplify rule type by extracting the value type from placed types and removing the Local type alias
         */
        @tailrec
        def simplifyRuleType(ruleType: Type, pos: Position): Type = {
          ruleType match {
            case ruleType
              if isParameterlessRule(ruleType)
                || isRuleFunctionOnRemoteSeq(ruleType)
                || isRuleFunctionOnRemoteSeqAndSelfReference(ruleType) =>
              ruleType
            case TypeRef(_, sym, List(valueType, _))
              if sym == symbols.on ||
                sym == symbols.from ||
                sym == symbols.fromSingle ||
                sym == symbols.fromMultiple =>
              simplifyRuleType(valueType, pos)
            case TypeRef(_, symbols.local, List(localValueType)) =>
              simplifyRuleType(localValueType, pos)
            case _ => c.abort(pos, s"Selection rule has incorrect type: $ruleType")
          }
        }

        /**
         * Normalizes the rule argument passed to `SelectAny.apply` to be of type `Remote[_]`,
         * instead of a function type, by applying the function to appropriate values.
         *
         * @param tree the application of the `SelectAny.apply` method
         */
        def normalizeToParameterlessRule(tree: Tree): Tree = {
          val q"$apply[$peerType]($ruleArg)" = tree: @unchecked
          val ruleType = simplifyRuleType(ruleArg.tpe, ruleArg.pos)
          ruleType match {
            case null => c.abort(ruleArg.pos, "rule parameter needs to be typed")
            case ruleType if isRuleFunctionOnRemoteSeq(ruleType) =>
              val normalizedRuleArg = normalizeRuleFunctionOnRemoteSeq(ruleArg, peerType.tpe, ruleType)
              normalizedCount += 1
              q"$apply[$peerType]($normalizedRuleArg)"
            case ruleType if isRuleFunctionOnRemoteSeqAndSelfReference(ruleType) =>
              val normalizedRuleArg = normalizeRuleFunctionOnRemoteSeqAndSelfReference(ruleArg, peerType.tpe, ruleType)
              normalizedCount += 1
              q"$apply[$peerType]($normalizedRuleArg)"
            case ruleType if isParameterlessRule(ruleType) =>
              tree
            case tpe =>
              c.abort(ruleArg.pos, s"rule parameter had unexpected type: $tpe")
          }
        }

        object transformer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case tree if isDynamicSelection(tree) =>
              processedCount += 1
              super.transform(normalizeToParameterlessRule(tree))
            case tree => super.transform(tree)
          }
        }

        val result = Initialized(ImplDefOps(tree).map { (mods, parents, self, body) =>
          (mods, parents, self, transformer.transformTrees(body))
        })

        logging.debug(s" Processed $processedCount dynamic selection rules and normalized $normalizedCount")
        result
    }
  }

  /**
   * Is the tree the application of a method belonging to [[symbols.SelectAny]]?
   * It needs to have a defined peerType type argument and a rule argument
   */
  private def isDynamicSelection(tree: Tree): Boolean = {
    tree match {
      case q"$apply[$peerType]($ruleArg)"
        if apply.nonEmpty &&
          apply.symbol != null &&
          apply.symbol.owner == symbols.SelectAny &&
          peerType.nonEmpty &&
          ruleArg.nonEmpty =>
        true
      case _ => false
    }
  }

}
