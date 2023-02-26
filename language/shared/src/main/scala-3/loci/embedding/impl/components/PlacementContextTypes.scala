package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.quoted.*

trait PlacementContextTypes:
  this: Component with Commons with ErrorReporter with Annotations with PlacementInfo =>
  import quotes.reflect.*

  object PlacementLiftingConversion:
    def unapply(term: Term): Option[Term] = term match
      case Inlined(Some(call), List(conversion: ValDef), erased @ Typed(Apply(_, List(rhs)), _))
        if call.symbol == symbols.placed.companionModule.moduleClass &&
           !(conversion.tpt.tpe =:= TypeRepr.of[Nothing]) && conversion.tpt.tpe <:< types.conversion &&
           !(erased.tpe =:= TypeRepr.of[Nothing]) && erased.tpe <:< types.placed =>
        rhs match
          case Typed(Repeated(List(Inlined(_, List(), rhs)), _), _) => Some(rhs)
          case Typed(Repeated(List(rhs), _), _) => Some(rhs)
          case Repeated(List(Inlined(_, List(), rhs)), _) => Some(rhs)
          case Repeated(List(rhs), _) => Some(rhs)
          case _ => Some(rhs)
      case Inlined(Some(call), List(conversion: ValDef, ValDef(_, _, Some(rhs))), erased @ Typed(Apply(_, List(_)), _))
        if call.symbol == symbols.placed.companionModule.moduleClass &&
           !(conversion.tpt.tpe =:= TypeRepr.of[Nothing]) && conversion.tpt.tpe <:< types.conversion &&
           !(erased.tpe =:= TypeRepr.of[Nothing]) && erased.tpe <:< types.placed =>
        rhs match
          case Inlined(_, List(), rhs) => Some(rhs)
          case _ => Some(rhs)
      case _ =>
        None

  private def contextMethodType[T: Type, R: Type] =
    val Inlined(_, _, Block(List(lambda), _)) = '{ (_: T) ?=> erased: R }.asTerm: @unchecked
    val tpe @ MethodType(_, _, _) = lambda.symbol.info: @unchecked
    tpe

  private def normalBody(symbol: Symbol, placementInfo: PlacementInfo, rhs: Term) =
//    given peer: Type[?] = placementInfo.peerType.asType
//    given placement: Type[?] = symbols.`embedding.on`.typeRef.appliedTo(placementInfo.canonicalType.typeArgs).asType
    val peer = placementInfo.peerType.asPackedValueType
    val placement = symbols.`embedding.on`.typeRef.appliedTo(placementInfo.canonicalType.typeArgs).asPackedValueType
    Lambda(symbol, contextMethodType[Placement.Context[peer.Type], placement.Type], (symbol, _) =>
      val erased = Typed(Ref(symbols.erased), TypeTree.of[placement.Type])
      rhs.changeOwner(symbol) match
        case expr @ Lambda(_, _) => Block(List(expr), erased)
        case Block(statements, expr) => Block(statements :+ expr, erased)
        case expr => Block(List(expr), erased))

  private def placedExpressionSyntaxInfo(rhs: Option[Term]) = rhs match
    case Some(Lambda(List(arg), Apply(Apply(fun, List(Lambda(_, _))), _))) if arg.symbol.isImplicit && fun.symbol.owner == symbols.on => (true, true)
    case Some(Apply(Apply(fun, List(Lambda(_, _))), _)) if fun.symbol.owner == symbols.on => (true, false)
    case _ => (false, false)

  private def stripPlacedExpressionSyntax(symbol: Symbol, placementInfo: PlacementInfo, rhs: Term) =
    val (rhsStripped, arg) = rhs match
      case Lambda(List(arg), Apply(Apply(fun, List(Lambda(_, rhs))), _)) if arg.symbol.isImplicit && fun.symbol.owner == symbols.on => rhs -> Some(arg)
      case Apply(Apply(fun, List(Lambda(List(arg), rhs))), _) if fun.symbol.owner == symbols.on => rhs -> Some(arg)
      case _ => rhs -> None
    normalBody(arg.fold(symbol) { _.symbol.owner.owner }, placementInfo, rhsStripped)

  private def stripPlacedExpressionSyntax(symbol: Symbol, placementInfo: PlacementInfo, rhs: Option[Term]): Option[Term] =
    rhs map { stripPlacedExpressionSyntax(symbol, placementInfo, _) }

  private def stripPlacementLiftingConversion(term: Term): Term = term match
    case Block(statements, PlacementLiftingConversion(expr)) => Block.copy(term)(statements, expr)
    case PlacementLiftingConversion(expr) => expr
    case _ => term

  private def stripPlacementLiftingConversion(symbol: Symbol, placementInfo: PlacementInfo, rhs: Term): Term =
    val (rhsStripped, arg) = rhs match
      case Lambda(List(arg), rhs) if arg.symbol.isImplicit => stripPlacementLiftingConversion(rhs) -> Some(arg)
      case _ => rhs -> None
    normalBody(arg.fold(symbol) { _.symbol.owner.owner }, placementInfo, rhsStripped)

  private def stripPlacementLiftingConversion(symbol: Symbol, placementInfo: PlacementInfo, rhs: Option[Term]): Option[Term] =
    rhs map { stripPlacementLiftingConversion(symbol, placementInfo, _) }

  private def noCanonicalTypeMessage(placementInfo: PlacementInfo) =
    s"Placement type should be given as: ${placementInfo.showCanonical}"

  private def noTypeInferenceMessage =
    "Placement type inference not supported with current compiler version. Type needs to be ascribed explicitly."

  private def normalizePlacementContextType(
      symbol: Symbol,
      tpt: TypeTree,
      rhs: Option[Term],
      pos: Position,
      normalizeBody: Boolean): (Option[PlacementInfo], Option[Term], Boolean) =
    val info = symbol.info
    val typeInferred = tpt match
      case Inferred() => true
      case _ => false
    val (placedExpressionSyntax, expressionInContextFunction) = placedExpressionSyntaxInfo(rhs)
    val placementInfo = PlacementInfo(info.resultType)

    if placementInfo.isEmpty && placedExpressionSyntax then
      errorAndCancel("Placement expression for value with no placement type.", pos)

    placementInfo.fold(None, rhs, false) { placementInfo =>
      if !placementInfo.canonical then
        if !typeInferred then
          errorAndCancel(noCanonicalTypeMessage(placementInfo), tpt.posInUserCode)
        SymbolMutator.get.fold(
          errorAndCancel(noTypeInferenceMessage, pos)):
          _.setInfo(symbol, info.withResultType(placementInfo.canonicalType))

      if !placementInfo.canonical && SymbolMutator.get.isEmpty then
        (None, rhs, false)
      else if placedExpressionSyntax then
        (Some(placementInfo),
         if normalizeBody then stripPlacedExpressionSyntax(symbol, placementInfo, rhs) else rhs,
         placementInfo.canonical || expressionInContextFunction)
      else
        (Some(placementInfo),
         if normalizeBody then stripPlacementLiftingConversion(symbol, placementInfo, rhs) else rhs,
         placementInfo.canonical)
    }
  end normalizePlacementContextType

  private def normalizePlacementContextType(stat: ValDef | DefDef, normalizeBody: Boolean): ValDef | DefDef = stat match
    case stat @ ValDef(name, tpt, _) =>
      val (placementInfo, rhs, _) = normalizePlacementContextType(stat.symbol, tpt, stat.rhs, stat.posInUserCode, normalizeBody)
      if normalizeBody then
        placementInfo.fold(stat): placementInfo =>
          val normalizedType = if placementInfo.canonical then tpt else TypeTree.of(using placementInfo.canonicalType.asType)
          ValDef.copy(stat)(name, normalizedType, rhs)
      else
        stat
    case stat @ DefDef(name, paramss, tpt, _) =>
      val (placementInfo, rhs, expressionInContextFunction) = normalizePlacementContextType(stat.symbol, tpt, stat.rhs, stat.posInUserCode, normalizeBody)
      if placementInfo exists { !_.modality.local } then
        paramss collectFirst Function.unlift(_.params find { _.symbol.isImplicit }) foreach: param =>
          errorAndCancel("Non-local placed values cannot have implicit arguments.", param.posInUserCode)
      if placementInfo.isDefined && !expressionInContextFunction then
        tryIncrementContextResultCount(stat.symbol)
      if normalizeBody then
        placementInfo.fold(stat): placementInfo =>
          val normalizedType = if placementInfo.canonical then tpt else TypeTree.of(using placementInfo.canonicalType.asType)
          DefDef.copy(stat)(name, paramss, normalizedType, rhs)
      else
        stat

  private def normalizePlacementContextType(term: Term, symbol: Symbol): Term =
    PlacementInfo(term.tpe.resultType).fold(term) { placementInfo =>
      if placementInfo.modality.subjective then
        val pos = term match
          case Apply(Apply(TypeApply(fun, _), _), _) => fun.posInUserCode
          case _ => term.posInUserCode
        errorAndCancel("Placed statements cannot be subjective.", pos)
      stripPlacedExpressionSyntax(symbol, placementInfo, term)
    }

  private object contextArgumentSynthesizer extends TreeMap:

    // TODO this is due to a compiler bug: https://github.com/lampepfl/dotty/issues/17003

    override def transformTypeTree(tree: TypeTree)(owner: Symbol) = tree match
      case tree: TypeBoundsTree =>
        TypeBoundsTree.copy(tree)(transformTypeTree(tree.low)(owner), transformTypeTree(tree.hi)(owner)).asInstanceOf[TypeTree]
      case _ =>
        super.transformTypeTree(tree)(owner)

    override def transformTerm(term: Term)(owner: Symbol) = term match
      case Apply(apply @ Select(qualifier, names.apply), List(arg))
        if apply.symbol.owner == symbols.contextFunction1 &&
           !(arg.tpe =:= TypeRepr.of[Nothing]) && arg.tpe <:< types.context =>
        transformTerm(qualifier)(owner)

      case _ =>
        val transformedTerm = super.transformTerm(term)(owner)
        val symbol = transformedTerm.symbol

        if symbol.exists && !symbol.isAnonymousFunction && isMultitierModule(symbol.owner) then
          symbol.tree match
            case ValOrDefDef(stat) => normalizePlacementContextType(stat, normalizeBody = false)
            case _ =>

          PlacementInfo(symbol.info.resultType).fold(transformedTerm): placementInfo =>
            if !placementInfo.canonical then
              errorAndCancel(s"${noCanonicalTypeMessage(placementInfo)}${if SymbolMutator.get.isEmpty then s"\n$noTypeInferenceMessage" else ""}", term.posInUserCode)
              transformedTerm
            else
              val peer = placementInfo.peerType.asPackedValueType
              Select.unique(transformedTerm, names.apply).appliedTo('{ Placement.Context.fallback[peer.Type] }.asTerm)
        else
          transformedTerm
    end transformTerm
  end contextArgumentSynthesizer

  private class NormalizedDefBodyProcessor(transform: Option[((ValDef, Symbol) => ValDef, (Term, TypeRepr, Symbol) => Term)]) extends TreeMap:
    def dropLastExpr(block: Block) = block.statements match
      case (term: Term) :: Nil => term
      case statements :+ (term: Term) => Block.copy(block)(statements, term)
      case statements => Block.copy(block)(statements, Literal(UnitConstant()))

    def appendExpr(original: Block)(term: Term, expr: Term) = term match
      case Lambda(_, _) => Block.copy(original)(List(term), expr)
      case block @ Block(statements, Literal(UnitConstant())) => Block.copy(block)(statements, expr)
      case block @ Block(statements, _) => Block.copy(block)(statements :+ block.expr, expr)
      case _ => Block.copy(original)(List(term), expr)

    override def transformTerm(term: Term)(owner: Symbol) = term match
      case Block(List(lambda @ DefDef(name, args @ List(TermParamClause(List(arg))), tpt, Some(block @ Block(_, erased: Typed)))), closure: Closure)
          if arg.symbol.isImplicit &&
             !(arg.symbol.info =:= TypeRepr.of[Nothing]) && arg.symbol.info <:< types.context &&
             erased.tpe.typeSymbol == symbols.`embedding.on` =>
        val body = dropLastExpr(block)
        transform match
          case Some(_, transform) =>
            val rhs = appendExpr(block)(transform(body, erased.tpe, lambda.symbol), erased)
            Block.copy(term)(List(DefDef.copy(lambda)(name, args, tpt, Some(rhs))), closure)
          case _ =>
            body

      // TODO: this "outer" stuff should be copied/duplicated depending on ome configuration argument

      case Block(List(lambda @ DefDef(name, List(clause @ TermParamClause(arg :: _)), tpt, Some(body))), closure: Closure)
          if arg.symbol.isImplicit =>
        val params = transform match
          case Some(transformArg, _) => clause.params map { transformArg(_, lambda.symbol) }
          case _ => clause.params
        val rhs = transformTerm(body)(lambda.symbol)
        Block.copy(term)(List(DefDef.copy(lambda)(name, List(TermParamClause(params)), tpt, Some(rhs))), closure)

      case _ =>
        term
  end NormalizedDefBodyProcessor

  def transformNormalizedExpression(term: Term, owner: Symbol, transformArg: (ValDef, Symbol) => ValDef, transform: (Term, TypeRepr, Symbol) => Term) =
    val processor = NormalizedDefBodyProcessor(transform = Some(transformArg, transform))
    processor.transformTerm(term)(owner)

  def extractNormalizedExpression(term: Term, owner: Symbol) =
    val processor = NormalizedDefBodyProcessor(transform = None)
    processor.transformTerm(term)(owner)

  def normalizePlacementContextTypes(module: ClassDef): ClassDef =
    val body = module.body map: stat =>
      val normalizedStat = stat match
        case ValOrDefDef(stat) => normalizePlacementContextType(stat, normalizeBody = true)
        case stat: Term => normalizePlacementContextType(stat, module.symbol)
        case _ => stat
      contextArgumentSynthesizer.transformStatement(normalizedStat)(module.symbol)

    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body)
  end normalizePlacementContextTypes
end PlacementContextTypes
