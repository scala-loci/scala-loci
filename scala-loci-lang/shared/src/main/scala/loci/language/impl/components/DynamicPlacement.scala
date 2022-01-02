package loci.language.impl.components

import loci.language.impl.Component
import loci.language.impl.Engine
import loci.language.impl.Phase

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.macros.blackbox

object DynamicPlacement extends Component.Factory[DynamicPlacement](
  requires = Seq(Commons, Initialization, GatewayAccess, ModuleInfo, Values)
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
    ),
    Phase(
      "dynamic:remotecall:recursive",
      recursiveDynamicallyPlacedRemoteCalls,
      after = Set("dynamic:selection:normalize", "remote:block"),
      before = Set("*", "values:collect")
    ),
    Phase(
      "dynamic:selfreference:validate",
      validateSelfReferences,
      after = Set("values:collect", "dynamic:self:reference")
    ),
    Phase(
      "dynamic:self:reference",
      replaceSelfReference,
      after = Set("values:collect")
    ),
    Phase(
      "dynamic:network:monitor",
      replaceNetworkMonitor,
      after = Set("values:collect")
    )
  )

  private val commons = engine.require(Commons)
  private val initialization = engine.require(Initialization)
  private val gatewayAccess = engine.require(GatewayAccess)
  private val moduleInfo = engine.require(ModuleInfo)
  private val values = engine.require(Values)

  import commons._
  import gatewayAccess._
  import initialization._
  import moduleInfo._
  import values._
  import engine.c
  import engine.c.universe._


  /**
   * This phase is executed before "remote:block".
   * All arguments passed to `SelectAny._` in multitier code are normalized to type `Remote[_]`.
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

        /**
         * A rule argument of type `Seq[Remote[P]] => Remote[P]` is replaced by its application on
         * `remote[P].connected`, which yields return type `Remote[P]`
         *
         * @param ruleFunction       the argument initially passed as selection rule
         * @param ruleOutputPeerType the type of peer that is selected
         * @param ruleType           the type of the selection rule
         */
        def normalizeRuleFunctionOnRemoteSeq(ruleFunction: Tree, ruleOutputPeerType: Type, ruleType: Type): Tree = {
          val gateway = createMultipleGateway(ruleOutputPeerType, ruleFunction.pos)
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
          val gateway = createMultipleGateway(ruleOutputPeerType, ruleFunction.pos)

          val selfReferenceType = ruleType.typeArgs(1)
          val selfReference = internal.setType(
            q"new ${names.root}.loci.runtime.Remote.SelfReference[${selfReferenceType.typeArgs.head}](${peerSignature(ruleOutputPeerType, ruleFunction.pos)})",
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
   * This phase is executed after "remote:block".
   * It transforms dynamically placed remote calls of the form `SelectAny.recursive[P](selection _).call(f(...))` into
   * calls of a new recursive function. The recursive function first calls the selection to get a remote reference.
   * If it is a SelfReference, f is called. If it is not, the recursive function is called remotely on the selected
   * remote reference.
   */
  def recursiveDynamicallyPlacedRemoteCalls(records: List[Any]): List[Any] = {
    records process {
      case Initialized(tree) =>
        val liftedRecursiveDefinitions = mutable.ListBuffer.empty[Tree]

        /**
         * Transform RefTrees in the given tree by swapping their names with new term names. Collect the newly
         * introduced term names together with their original RefTrees.
         * @param tree the tree to be transformed
         * @param replacedNamePrefix the prefix of the newly introduced term names, appended by indexes
         * @return list of original RefTree -> new term name, transformed tree
         */
        def transformUnboundReferences(tree: Tree, replacedNamePrefix: String): (List[(RefTree, TermName)], Tree) = {
          val extractedRefsWithNewNames = mutable.ListBuffer.empty[(RefTree, TermName)]

          case class UnboundReferenceTransformer(knownParams: Set[Name]) extends Transformer {
            override def transform(tree: Tree): Tree = tree match {
              case tree @ Function(params, _) if params.forall(param => knownParams.contains(param.name.decodedName)) =>
                super.transform(tree)
              case tree @ Function(params, _) =>
                UnboundReferenceTransformer(params.map(_.name.decodedName).toSet).transform(tree)
              case tree @ RefTree(qualifier, name)
                if tree.symbol != NoSymbol &&
                  isLocalIdentifier(tree.symbol.owner) &&
                  !knownParams.contains(name) =>
                val newName = TermName(s"$replacedNamePrefix${extractedRefsWithNewNames.length}")
                extractedRefsWithNewNames.append(tree -> newName)
                treeCopy.RefTree(tree, qualifier, newName)
              case _ => super.transform(tree)
            }
          }

          val treeWithReplacedReferences = UnboundReferenceTransformer(Set.empty[Name]).transform(tree)
          (extractedRefsWithNewNames.toList, treeWithReplacedReferences)
        }

        /**
         * Lift the recursively placed remote call to a call of a new recursive function remotely calling itself when a
         * remote reference is selected. Add this recursive function to the module.
         *
         * @param tree the original remote call to be lifted
         * @param recursiveCallAccessor a remote accessor to be wrapped around the recursive remote call in the created function;
         *                              only defined if the original remote call is accessed via remote accessor (i.e.
         *                              in [[liftDynamicallyPlacedRemoteCallInsideRemoteAccessor]]
         * @param recursiveCallAccessorIsBlocking Is recursiveCallAccessor blocking? If not the recursive function will return a Future
         * @return the lifted call to the new recursive function
         */
        def liftDynamicallyPlacedRemoteCall(
          tree: Tree,
          recursiveCallAccessor: Option[Tree => Tree] = None,
          recursiveCallAccessorIsBlocking: Option[Boolean] = None
        ): Tree = {
          require(recursiveCallAccessor.isDefined == recursiveCallAccessorIsBlocking.isDefined)

          val q"$expr.$call[..$callTpts](...$exprss)" = tree: @unchecked
          val q"$selectAny.recursive[$peerType]($ruleArg)" = expr: @unchecked
          val callExpr: Tree = exprss.head.head
          val q"$calledFunction[..$tpts](...$callArgs)" = callExpr: @unchecked

          val liftedNameIndex = liftedRecursiveDefinitions.length
          val liftedName = TermName(s"$$loci$$dynamic$$remotecall$$$liftedNameIndex")
          val liftedSymbol = internal.newMethodSymbol(
            module.classSymbol,
            liftedName,
            callExpr.pos,
            Flag.SYNTHETIC | Flag.PRIVATE | Flag.LOCAL
          )

          val liftedArgs = callArgs.asInstanceOf[List[List[Tree]]].zipWithIndex.map {
            case (callArgsSublist, outerIndex) => callArgsSublist.zipWithIndex.map {
              case (callArg, innerIndex) =>
                val liftedParamName = TermName(s"$$loci$$dynamic$$remotecall$$$liftedNameIndex$$param$$$outerIndex$$$innerIndex")
                internal.setInfo(
                  internal.newTermSymbol(liftedSymbol, liftedParamName, callArg.pos, Flag.PARAM),
                  callArg.tpe.underlying // transform ConstantType into its non-constant equivalent
                ) -> callArg
            }
          }

          val (unboundRefs, ruleArgWithReplacedReferences) = transformUnboundReferences(
            ruleArg,
            s"$$loci$$dynamic$$remotecall$$$liftedNameIndex$$ref$$"
          )
          val unboundRefArgs = unboundRefs.zipWithIndex.map {
            case ((ref, refParamName), index) =>
              internal.setInfo(
                internal.newTermSymbol(liftedSymbol, refParamName, ref.pos, Flag.PARAM),
                ref.tpe
              ) -> ref
          }

          val liftedTpe = internal.methodType(
            (liftedArgs.flatten ++ unboundRefArgs).map(_._1),
            types.on mapArgs { _ =>
              val Seq(value, peer) = extractTag(callExpr.tpe, types.placedValue, callExpr.pos).typeArgs: @unchecked
              if (value real_<:< types.per) {
                c.abort(callExpr.pos, "Recursive selection of subjective remote block currently not supported")
              }
              val dispatchedValue = recursiveCallAccessorIsBlocking match {
                case Some(false) => types.future mapArgs { _ => List(value) }
                case Some(true) => value
                case None => definitions.UnitTpe
              }
              List(dispatchedValue, peer)
            }
          )
          internal.setInfo(liftedSymbol, liftedTpe)

          val liftedParams = liftedArgs.map(_.map{
            case (symbol, tree) =>
              internal.setSymbol(
                q"${Modifiers(Flag.PARAM)} val ${symbol.name}: ${createTypeTree(tree.tpe, tree.pos)}",
                symbol
              )
          })
          val unboundRefParams = unboundRefArgs.map {
            case (symbol, tree) =>
              internal.setSymbol(
                q"${Modifiers(Flag.PARAM)} val ${symbol.name}: ${createTypeTree(tree.tpe, tree.pos)}",
                symbol
              )
          }

          val liftedResult = createTypeTree(liftedTpe.resultType, callExpr.pos)

          val moduleThis = internal.setSymbol(q"${module.className}.this", module.symbol.info.baseClasses.head)
          val liftedCall = q"$moduleThis.$liftedName(..${callArgs.asInstanceOf[List[List[Tree]]].flatten ++ unboundRefs.map(_._1)})"
          internal.setSymbol(liftedCall, liftedSymbol)
          internal.setType(liftedCall, liftedTpe.resultType)

          val syntheticName = TermName("$loci$synthetic")
          val placedName = TermName("placed")
          val placedSymbol = internal.newMethodSymbol(symbols.Placed, placedName, flags = Flag.SYNTHETIC)
          val placed = internal.setSymbol(q"$syntheticName.$placedName", placedSymbol)
          val placedContextTypeTree: Tree = createTypeTree(types.context.mapArgs(_ => List(liftedTpe.resultType.typeArgs.last)), callExpr.pos)
          val placedContextParam = internal.setPos(
            ValDef(Modifiers(Flag.IMPLICIT | Flag.PARAM), TermName("$bang"), placedContextTypeTree, EmptyTree),
            callExpr.pos
          )

          val calledFunctionApplication = recursiveCallAccessorIsBlocking match {
            case Some(false) =>
              q"${trees.futureSuccessful}($calledFunction[..$tpts](...${liftedParams.map(_.map(_.symbol.name))}))"
            case _ =>
              q"$calledFunction[..$tpts](...${liftedParams.map(_.map(_.symbol.name))})"
          }

          val innerLiftedCall = q"$moduleThis.$liftedName(..${(liftedParams.flatten ++ unboundRefParams).map(_.symbol.name)})"
          internal.setSymbol(innerLiftedCall, liftedSymbol)
          internal.setType(innerLiftedCall, liftedTpe.resultType)
          val remoteIdentifier = TermName("selectedRemote")

          val remoteApplySymbol = internal.newMethodSymbol(symbols.Select, TermName("apply"))
          val remoteIdentifierExpr = internal.setType(q"$remoteIdentifier", types.remote)
          val remoteApply = internal.setSymbol(
            q"${trees.remote}[$peerType]($remoteIdentifierExpr)",
            remoteApplySymbol
          )
          val recursiveCall = internal.setSymbol(
            internal.setType(
              q"$remoteApply.$call[..$callTpts]($innerLiftedCall)",
              tree.tpe
            ),
            tree.symbol
          )
          val accessedRecursiveCall = recursiveCallAccessor.map(_(recursiveCall)).getOrElse(recursiveCall)

          val localExecutionOrRecursiveCall =
            q"""
              val $remoteIdentifier = $ruleArgWithReplacedReferences
              $remoteIdentifier match {
                case remote: ${types.selfReference} =>
                  $calledFunctionApplication
                case _ =>
                  $accessedRecursiveCall
              }
             """

          internal.setType(localExecutionOrRecursiveCall, liftedTpe.resultType)
          val liftedBody = internal.setType(
            q"$placed((..$placedContextParam) => $localExecutionOrRecursiveCall)",
            liftedTpe.resultType
          )

          val liftedDefinition = q"${Flag.SYNTHETIC} private[this] def $liftedName(..${liftedParams.flatten ++ unboundRefParams}): $liftedResult = $liftedBody"
          internal.setSymbol(liftedDefinition, liftedSymbol)
          internal.setType(liftedDefinition, liftedTpe)

          liftedRecursiveDefinitions += atPos(callExpr.pos)(liftedDefinition)

          liftedCall
        }

        /**
         * Lift the recursively placed remote call to a call of a new recursive function that remotely calls itself if
         * a remote reference is selected. Add this recursive function to the module. The original remote call must be
         * in a remote accessor.
         *
         * @param tree the original remote call inside a remote accessor
         * @return the lifted call to the new recursive function
         */
        def liftDynamicallyPlacedRemoteCallInsideRemoteAccessor(tree: Tree): Tree = {
          val q"$accessorExpr.$asLocal" = tree: @unchecked
          val q"$accessor(...$exprss)" = accessorExpr: @unchecked
          val transmission: Tree = exprss(1).head

          val isBlocking: Boolean = accessorExpr.tpe real_<:< types.blockingRemoteAccessor

          val recursiveCallAccessor = (recursiveCall: Tree) => {
            val newAccessor = internal.setType(
              q"$accessor($recursiveCall)($transmission)",
              accessorExpr.tpe
            )
            internal.setType(q"$newAccessor.$asLocal", tree.tpe)
          }

          val liftedCall = liftDynamicallyPlacedRemoteCall(
            exprss.head.head,
            Option(recursiveCallAccessor),
            Option(isBlocking)
          )
          atPos(accessorExpr.pos) {
            liftedCall
          }
        }

        object transformer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case tree if isDynamicallyPlacedRemoteCallWithRecursiveSelectionInsideRemoteAccessor(tree) =>
              super.transform(liftDynamicallyPlacedRemoteCallInsideRemoteAccessor(tree))
            case tree if isDynamicallyPlacedRemoteCallWithRecursiveSelection(tree) =>
              super.transform(liftDynamicallyPlacedRemoteCall(tree))
            case tree => super.transform(tree)
          }
        }

        val result = Initialized(ImplDefOps(tree).map { (mods, parents, self, body) =>
          (mods, parents, self, transformer.transformTrees(body) ++ liftedRecursiveDefinitions)
        })

        // add the generated lifted definitons to the module's scope
        def classInfoType(parents: List[Type], decls: Scope, typeSymbol: Symbol): ClassInfoType = {
          val scope = internal.newScopeWith(decls.toSeq ++ (liftedRecursiveDefinitions map { _.symbol }): _*)
          internal.classInfoType(parents, scope, typeSymbol)
        }

        val info = (module.classSymbol.info: @unchecked) match {
          case PolyType(typeParams, ClassInfoType(parents, decls, typeSymbol)) =>
            internal.polyType(typeParams, classInfoType(parents, decls, typeSymbol))
          case ClassInfoType(parents, decls, typeSymbol) =>
            classInfoType(parents, decls, typeSymbol)
        }

        internal.setInfo(module.classSymbol, info)

        logging.debug(s" Expanded ${liftedRecursiveDefinitions.size} dynamically placed remote calls recursively")
        result
    }
  }

  def replaceSelfReference(records: List[Any]): List[Any] = {
    logging.debug(s" Replacing calls to erased self reference with runtime SelfReference")
    var count = 0

    object transformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case tree @ q"_root_.loci.`package`.self[$peer]($_)" if tree.tpe real_<:< types.selfReference =>
          count += 1
          val selfReference = internal.setType(
            q"new ${names.root}.loci.runtime.Remote.SelfReference[$peer](${peerSignature(peer.tpe, peer.pos)})",
            tree.tpe
          )
          super.transform(selfReference)
        case tree => super.transform(tree)
      }
    }

    val result = records process {
      case v: Value => v.copy(tree = transformer.transform(v.tree))
    }

    logging.debug(s" Replaced $count calls to erased self reference with runtime SelfReference")
    result
  }

  /**
   * This phase is executed after "values:collect".
   * It validates that SelfReferences are only created were they are legal by their type, i.e. a `SelfReference[A]` may
   * only be created on a peer `B` if `B <: A`. Also SelfReferences may not be created in ModuleValues.
   * Aborts compilation if a SelfReference violates these assumptions.
   */
  def validateSelfReferences(records: List[Any]): List[Any] = {
    logging.debug(s" Validating creations of SelfReference to fulfill type constraints")

    case class SelfReferencePeerTypeValidator(peer: Option[Type]) extends Traverser {
      override def traverse(tree: Tree): Unit = tree match {
        case tree @ q"new $_[..$_](...$_)" if tree.tpe real_<:< types.selfReference =>
          peer match {
            case Some(peer) if peer real_<:< tree.tpe.typeArgs.head =>
            case Some(peer) => c.abort(
              tree.pos,
              s"SelfReference of type ${tree.tpe.typeArgs.head} appeared on peer $peer, which is not a subtype of ${tree.tpe.typeArgs.head}"
            )
            case None => c.abort(
              tree.pos,
              s"SelfReference of type ${tree.tpe.typeArgs.head} appeared on a non-placed value"
            )
          }
        case tree => super.traverse(tree)
      }
    }

    records foreach {
      case PlacedValue(_, tree, peer, _) =>
        SelfReferencePeerTypeValidator(peer.map(_.asType.toType.asSeenFrom(module.classSymbol))).traverse(tree)
      case ModuleValue(_, tree) =>
        SelfReferencePeerTypeValidator(None).traverse(tree)
      case _ =>
    }

    records
  }

  def replaceNetworkMonitor(records: List[Any]): List[Any] = {
    logging.debug(s" Replacing calls to erased network monitor with runtime network monitor")
    var count = 0

    object transformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case tree @ q"_root_.loci.`package`.networkMonitor" if tree.tpe real_<:< types.networkMonitor =>
          count += 1
          val systemNetworkMonitorTree = q"$$loci$$sys.getNetworkMonitor"
          internal.setType(systemNetworkMonitorTree, types.networkMonitor)
          super.transform(systemNetworkMonitorTree)
        case tree => super.transform(tree)
      }
    }

    val result = records process {
      case v: Value => v.copy(tree = transformer.transform(v.tree))
    }

    logging.debug(s" Replaced $count calls to erased network monitor with runtime network monitor")
    result
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

  private def isRecursiveDynamicSelection(tree: Tree): Boolean = {
    isDynamicSelection(tree) && (tree match {
      case q"$expr.$recursive[$peerType]($ruleArg)" =>
        recursive == names.recursive
      case _ => false
    })
  }

  private def isDynamicallyPlacedRemoteCallWithRecursiveSelection(tree: Tree): Boolean = {
    tree match {
      case tree @ q"$expr.$call[..$_](...$exprss)"
        if tree.nonEmpty &&
          tree.symbol != null &&
          tree.symbol.owner == symbols.Call &&
          exprss.nonEmpty =>
        isRecursiveDynamicSelection(expr)
      case _ => false
    }
  }

  private def isDynamicallyPlacedRemoteCallWithRecursiveSelectionInsideRemoteAccessor(tree: Tree): Boolean = {
    tree match {
      case q"$expr.$anyAsLocal(...$_)" => expr match {
        case tree @ q"$accessor(...$exprss)" if tree.nonEmpty && (tree.tpe real_<:< types.remoteAccessor) && exprss.nonEmpty && exprss.head.nonEmpty =>
          isDynamicallyPlacedRemoteCallWithRecursiveSelection(exprss.head.head)
        case _ => false
      }
      case _ => false
    }
  }

}
