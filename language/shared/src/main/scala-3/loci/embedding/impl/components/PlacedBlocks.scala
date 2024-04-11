package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.collection.mutable
import scala.collection.immutable.ListMap

@experimental
trait PlacedBlocks:
  this: Component & Commons & ErrorReporter & Annotations & Placements =>
  import quotes.reflect.*

  private object Selection:
    def unapply(term: Term) = term match
      case Apply(TypeApply(Select(_, _), remoteTypeTree :: _), remotes)
          if term.symbol.maybeOwner == symbols.select =>
        Some(remoteTypeTree, remotes)
      case _ =>
        None

  private object Run:
    def unapply(term: Term) = term match
      case Apply(TypeApply(Select(prefix, _), _), _) if prefix.tpe derivesFrom symbols.run =>
        prefix match
          case Selection(remoteTypeTree, remotes) => Some(Some(remoteTypeTree, remotes))
          case _ => Some(None)
      case _ => None

  private object Capture:
    def unapply(term: Term) = term match
      case Apply(Select(Run(selection), _), captures) if term.symbol.maybeOwner == symbols.capture =>
        captures match
          case List(Typed(Repeated(captures, _), _)) => Some(selection, captures)
          case _ => Some(selection, captures)
      case _ =>
        None

  private object PlacedBlock:
    def unapply(term: Term) = term match
      case Apply(Apply(TypeApply(Select(prefix, name), _ :: value :: _), List(lambda @ Lambda(List(arg), block))), _)
          if term.symbol.maybeOwner == symbols.block && lambda.tpe.isContextFunctionType =>
        val List(remote, local, from) = prefix.tpe.baseType(symbols.block).typeArgs: @unchecked
        prefix match
          case Run(selection) => Some(selection, List.empty, value.tpe, remote, local, from, name != names.apply, block, arg.symbol.owner)
          case Capture(selection, captures) => Some(selection, captures, value.tpe, remote, local, from, name != names.apply, block, arg.symbol.owner)
          case _ => None
      case _ => None

  private object PlacedBlockInvocation:
    def unapply(term: Term) =
      val (expr, access) = term match
        case PlacedAccess(term, arg, typeApplies, apply, prefix, transmission, suffix) =>
          (arg, Some(term, arg, typeApplies, apply, prefix, transmission, suffix))
        case _ =>
          (term, None)
      expr match
        case PlacedBlock(selection, captures, value, remote, local, from, subjective, expr, context) =>
          Some(access, selection, captures, value, remote, local, from, subjective, expr, context)
        case _ =>
          None

  private object NestedTypeApplies:
    def unapply(term: Term): Some[Term] = term match
      case TypeApply(NestedTypeApplies(fun), _) => Some(fun)
      case _ => Some(term)

  private object EtaExpansion:
    def unapply(term: Term): Option[Term] = term match
      case Lambda(lambdaArgs, EtaExpansion(NestedTypeApplies(Apply(NestedTypeApplies(fun), funArgs)))) =>
        Option.when((lambdaArgs map { _.symbol }) == (funArgs map { _.symbol })) { fun }
      case Lambda(lambdaArgs, NestedTypeApplies(Apply(NestedTypeApplies(fun), funArgs))) =>
        Option.when((lambdaArgs map { _.symbol }) == (funArgs map { _.symbol })) { fun }
      case _ =>
        None

  private object LocalRef:
    def unapply(term: Term): Option[Term] = term match
      case Ident(_) | Select(This(_), _) => Some(term)
      case _ => None

  def liftRemoteBlocks(module: ClassDef): ClassDef =
    val blockMethods = mutable.ListBuffer.empty[DefDef]
    var blockIndex = 0

    def isLocalVariable(symbol: Symbol) =
      symbol.maybeOwner != module && (symbol hasAncestor module.symbol)

    def subsitituteVariables(term: Term, owner: Symbol, substitutions: Map[Symbol, Symbol]) =
      object variableSubstitutor extends SafeTreeMap(quotes):
        override def transformTerm(term: Term)(owner: Symbol) = term match
          case LocalRef(_) =>
            substitutions.get(term.symbol).fold(super.transformTerm(term)(owner)) { Ref(_) }
          case PlacedValueReference(LocalRef(value), _) =>
            substitutions.get(value.symbol).fold(super.transformTerm(term)(owner)) { Ref(_) }
          case _ =>
            super.transformTerm(term)(owner)

      variableSubstitutor.transformTerm(term)(owner).substituteRefs(substitutions filter { (from, _) => isLocalVariable(from) }, owner)
    end subsitituteVariables

    object variableCollector extends TreeAccumulator[(Symbol, Map[Symbol, Position])]:
      def foldTree(parameters: (Symbol, Map[Symbol, Position]), tree: Tree)(owner: Symbol) =
        val (context, variables) = parameters
        tree match
          case Ident(_) if isLocalVariable(tree.symbol) && !(tree.symbol hasAncestor context) =>
            foldOverTree((context, variables + (tree.symbol -> tree.posInUserCode)), tree)(owner)
          case _ =>
            foldOverTree(parameters, tree)(owner)

    object transformer extends SafeTreeMap(quotes):
      override def transformTerm(term: Term)(owner: Symbol) = term match
        case PlacedBlockInvocation(access, selection, captures, value, remote, local, from, subjective, expr, context) =>
          val captured = captures.foldLeft(ListMap.empty[Symbol, (TypeRepr, Term)]): (captured, capture) =>
            val entry = capture match
              case LocalRef(_) =>
                Some(capture.symbol -> (capture.symbol.info, capture))
              case PlacedValueReference(LocalRef(value), placementInfo) =>
                if placementInfo.modality.subjective then
                  errorAndCancel("Subjective placed values cannot be captured.", capture.posInUserCode)
                Some(value.symbol -> (placementInfo.valueType, capture))
              case _ =>
                val symbol = capture match
                  case EtaExpansion(LocalRef(capture)) => capture.symbol
                  case NestedTypeApplies(LocalRef(capture)) => capture.symbol
                  case _ => Symbol.noSymbol
                if symbol.isMethod then
                  errorAndCancel("Methods cannot be captured.", capture.posInUserCode)
                else
                  errorAndCancel("Identifier expected in capture clause.", capture.posInUserCode)
                None
            entry.fold(captured): entry =>
              val (symbol, _) = entry
              if captured contains symbol then
                errorAndCancel("Duplicate identifier in capture clause.", capture.posInUserCode)
              captured + entry

          val (_, variables) = variableCollector.foldTree((context, Map.empty), expr)(context)

          (variables -- captured.keys).headOption foreach: (symbol, pos) =>
            errorAndCancel(
               "Remote variable not captured. " +
               "Consider adding the identifier to the `capture` clause on the surrounding `run` construct: " +
              s"run.capture(${symbol.name})", pos)

          val paramNames = (captured.keys map { _.name }).toList
          val paramTypes = (captured.values map { (tpe, _) => tpe }).toList
          val paramRefs = (captured.values map { (_, expr) => expr }).toList

          def coerce(expr: Term) = expr match
            case _ if expr.tpe <:< TypeRepr.of[Unit] => expr
            case Lambda(_, _) => Block(List(expr), Literal(UnitConstant()))
            case Block(stats, expr) => Block(stats :+ expr, Literal(UnitConstant()))
            case _ => Block(List(expr), Literal(UnitConstant()))

          val coercedExpr =
            if access.isEmpty then
              if subjective then
                def tpe(name: String) = MethodType(List(name))(_ => List(symbols.remote.typeRef.appliedTo(local)), _ => TypeRepr.of[Unit])
                expr match
                  case _ if expr.tpe <:< TypeRepr.of[? => Unit] =>
                    expr
                  case Lambda(List(arg), expr) =>
                    Lambda(context, tpe(arg.name), (symbol, args) =>
                      coerce(expr.changeOwner(symbol).substituteRefs(arg.symbol, args.head.symbol, owner)))
                  case _ =>
                    Lambda(context, tpe("remote"), (symbol, args) =>
                      coerce(expr.changeOwner(symbol).select(symbols.function1Apply).appliedTo(Ref(args.head.symbol))))
              else
                coerce(expr)
            else
              expr

          val coercedType = if access.isEmpty then TypeRepr.of[Unit] else value

          val placed = if subjective then symbols.`language.per`.typeRef.appliedTo(List(coercedType, local)) else coercedType
          val languageType = symbols.`language.on`.typeRef.appliedTo(List(placed, remote))
          val embeddingType = symbols.`embedding.on`.typeRef.appliedTo(List(placed, remote))

          val name = s"$$loci$$anon$$$blockIndex"
          blockIndex += 1

          val symbol = newMethod(
            module.symbol,
            name,
            MethodType(paramNames)(_ => paramTypes, _ => languageType),
            Flags.PrivateLocal | Flags.Synthetic,
            Symbol.noSymbol)

          trySetContextResultCount(symbol, 1)

          val erased = Typed(Ref(symbols.erased), TypeTree.of(using embeddingType.asType))

          def erase(term: Term) = term match
            case Lambda(_, _) => Block(List(term), erased)
            case Block(stats, expr) => Block(stats :+ expr, erased)
            case _ => Block(List(term), erased)

          val index = blockMethods.size

          val peer = remote.asPackedValueType
          val placement = embeddingType.asPackedValueType
          val tpe = contextMethodType[Placement.Context[peer.Type], placement.Type]
          val block @ Block(List(lambda: DefDef), closure @ Closure(meth, _)) =
            Lambda(symbol, tpe, (symbol, _) => erase(transformTerm(coercedExpr)(context).changeOwner(symbol))): @unchecked
          val rhs = Block.copy(block)(List(lambda), Closure.copy(closure)(meth, Some(languageType)))

          val definition = DefDef(
            symbol,
            params => Some(subsitituteVariables(rhs, owner, (captured.keys zip (params.head map { _.symbol })).toMap)))

          blockMethods.insert(index, definition)

          def select(term: Term, remoteTypeTree: TypeTree, remotes: List[Term]) =
            term.appliedToTypeTrees(List(remoteTypeTree, TypeTree.of[Nothing])).appliedToArgs(remotes)

          val selected = selection match
            case Some(remoteTypeTree, remotes @ List(_)) if from.typeSymbol == symbols.fromMultiple =>
              select(Ref(symbols.remoteApply.owner.companionModule).select(symbols.selectApplySeq), remoteTypeTree, remotes)
            case Some(remoteTypeTree, remotes @ List(_)) if from.typeSymbol == symbols.fromSingle =>
              select(Ref(symbols.remoteApply.owner.companionModule).select(symbols.selectApplySingle), remoteTypeTree, remotes)
            case Some(remoteTypeTree, remotes) =>
              select(Ref(symbols.remoteApply.owner.companionModule).select(symbols.selectApplyMultiple), remoteTypeTree, remotes)
            case _ =>
              Ref(symbols.remoteApply).appliedToType(remote)

          val call = selected
            .select(symbols.callApply)
            .appliedToTypes(List(remote, remote, placed, symbols.`embedding.on`.typeRef))
            .appliedTo(Ref(symbol).appliedToArgs(paramRefs).select(symbols.contextFunction1Apply).appliedTo('{ Placement.Context.fallback[peer.Type] }.asTerm))
            .appliedTo(Ref(symbols.erased))

          val callAccess = access match
            case Some(term, _, typeApplies, apply, prefix, transmission, suffix) =>
              PlacedAccess(term, call, typeApplies, apply, prefix, transmission, suffix)
            case _ =>
              call

          super.transformTerm(callAccess)(owner)

        case _ =>
          super.transformTerm(term)(owner)

    val body = module.body flatMap: statement =>
      val statements = transformer.transformStatement(statement)(Symbol.spliceOwner) :: blockMethods.toList
      blockMethods.clear()
      statements

    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body)
  end liftRemoteBlocks
end PlacedBlocks
