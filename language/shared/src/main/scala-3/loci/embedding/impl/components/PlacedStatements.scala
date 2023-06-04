package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.quoted.*

@experimental
trait PlacedStatements:
  this: Component & Commons & ErrorReporter & Placements & Peers =>
  import quotes.reflect.*

  private def cleanPlacementExpression(placementInfo: PlacementInfo, expr: Term) =
    object PlacementSyntaxPrefix:
      def unapply(term: Term): Option[(ValDef, Term)] = term match
        case Inlined(Some(call), (prefix: ValDef) :: _, body) if call.symbol == symbols.placedExpression =>
          Some(prefix -> body)
        case _ =>
          None

    object PlacementSyntaxBody:
      def unapply(term: Term): Option[(ValDef, Term)] = term match
        case outer @ Block(List(Inlined(_, List(), inner @ Block((evidence: ValDef) :: _, _))), erased: Typed)
          if !(evidence.tpt.tpe =:= TypeRepr.of[Nothing]) &&
             evidence.tpt.tpe <:< types.context &&
             (erased.symbol == symbols.erased || erased.symbol == symbols.erasedArgs) =>
          Some(evidence -> Block.copy(outer)(inner.statements.tail :+ inner.expr, erased))
        case _ =>
          None

    def tryBetaReduce(expr: Term) = Term.betaReduce(expr) getOrElse expr

    val (cleanExpr, erasedSymbol, expressionOwner) = expr match
      case Apply(select @ Select(PlacementSyntaxPrefix(prefix, expr), names.apply), List(arg)) =>
        tryBetaReduce(expr.select(select.symbol).appliedTo(arg)) match
          case PlacementSyntaxBody(evidence, expr) => (expr, Some(evidence.symbol), Some(prefix.symbol.owner))
          case expr => (expr, None, Some(prefix.symbol.owner))
      case _ => tryBetaReduce(expr) match
        case PlacementSyntaxPrefix(prefix, expr) => expr match
          case PlacementSyntaxBody(evidence, expr) => (expr, Some(evidence.symbol), Some(prefix.symbol.owner))
          case _ => (expr, None, Some(prefix.symbol.owner))
        case _ => (expr, None, None)

    def erasedContext = Typed(
      Ref(symbols.erased),
      TypeTree.of(using symbols.`embedding.on`.typeRef.appliedTo(placementInfo.canonicalType.typeArgs).asType))

    cleanExpr match
      case Block(_, erased: Typed) if erased.expr.symbol == symbols.erased || erased.expr.symbol == symbols.erasedArgs =>
        (cleanExpr, erasedSymbol, expressionOwner)
      case Block(stats, expr) =>
        (Block(stats :+ expr, erasedContext), erasedSymbol, expressionOwner)
      case expr =>
        (Block(List(expr), erasedContext), erasedSymbol, expressionOwner)
  end cleanPlacementExpression

  private def cleanPlacementExpressionOrClosure(placementInfo: PlacementInfo, expr: Term) =
    expr match
      case block @ Lambda(List(arg), expr)
        if !(arg.tpt.tpe =:= TypeRepr.of[Nothing]) &&
           arg.tpt.tpe <:< types.context &&
           arg.symbol.isImplicit =>
        val Block(List(lambda: DefDef), closure) = block: @unchecked
        val (body, erasedSymbol, expressionOwner) = cleanPlacementExpression(placementInfo, lambda.rhs.get)
        (Block.copy(block)(
          List(DefDef.copy(lambda)(lambda.name, lambda.paramss, lambda.returnTpt, Some(body.changeOwner(lambda.symbol)))),
          closure),
         erasedSymbol,
         expressionOwner.nonEmpty)
      case _ =>
        val (body, erasedSymbol, expressionOwner) = cleanPlacementExpression(placementInfo, expr)
        expressionOwner.fold(body, erasedSymbol, false): expressionOwner =>
          val peer = placementInfo.peerType.asPackedValueType
          val placement = symbols.`embedding.on`.typeRef.appliedTo(placementInfo.canonicalType.typeArgs).asPackedValueType
          val tpe = contextMethodType[Placement.Context[peer.Type], placement.Type]
          val block @ Block(List(lambda: DefDef), closure @ Closure(meth, _)) =
            Lambda(expressionOwner, tpe, (symbol, _) => body.changeOwner(symbol)): @unchecked
          (Block.copy(block)(List(lambda), Closure.copy(closure)(meth, Some(placementInfo.canonicalType))), erasedSymbol, true)

  private class ErasedSymbolCleaner(symbol: Symbol) extends TreeMap:
    val peerType = symbol.info.baseType(types.context.typeSymbol).typeArgs.head.asType

    override def transformTerm(term: Term)(owner: Symbol) =
      if term.symbol == symbol then
        peerType match
          case '[ p ] => '{ Placement.Context.fallback[p] }.asTerm
      else
        super.transformTerm(term)(owner)
  end ErasedSymbolCleaner

  private def cleanPlacementSyntax(placementInfo: PlacementInfo, rhs: Term)(owner: Symbol): (Term, Boolean) =
    cleanPlacementExpressionOrClosure(placementInfo, rhs) match
      case (rhs, Some(erasedSymbol), placedExpressionSyntax) =>
        ErasedSymbolCleaner(erasedSymbol).transformTerm(rhs)(owner) -> placedExpressionSyntax
      case (rhs, _, placedExpressionSyntax) =>
        rhs -> placedExpressionSyntax

  private def cleanPlacementSyntax(placementInfo: PlacementInfo, rhs: Option[Term])(owner: Symbol): Option[Term] =
    rhs map: rhs =>
      val (expr, _) = cleanPlacementSyntax(placementInfo, rhs)(owner)
      expr

  private def placementType(stat: ValDef | DefDef, tpt: TypeTree) =
    PlacementInfo(stat.symbol.info.resultType) filter: placementInfo =>
      if !placementInfo.canonical then
        val (message, pos) = tpt match
          case Inferred() => "Placement type could not be inferred. Explicit type ascription required" -> stat.posInUserCode.startPosition
          case _ => "Invalid placement type. Placement types imported by loci.language.* required" -> tpt.posInUserCode
        errorAndCancel(s"$message: ${placementInfo.showCanonical}", pos)
      placementInfo.canonical

  private def checkPeerType(stat: Statement, peerType: TypeRepr, module: ClassDef, statement: String, relation: String): Unit =
    if PeerInfo(peerType).isEmpty then
      errorAndCancel(
        s"$statement must be $relation a peer type but is $relation ${peerType.safeShow}",
        stat.posInUserCode.startPosition)
    if peerType.typeSymbol.owner != module.symbol then
      errorAndCancel(
        s"$statement must be $relation a peer of module ${fullName(module.symbol)} " +
        s"but is $relation a peer in module ${fullName(peerType.typeSymbol.owner)}",
        stat.posInUserCode.startPosition)

  private def checkPlacementType(stat: Statement, placementInfo: PlacementInfo, module: ClassDef): Unit =
    val (statement, subjectiveStatement) = stat match
      case _: ValDef | _: DefDef => ("Placed definition", "Subjective placed definition")
      case _ => ("Placed statement", "Subjective placed statement")
    checkPeerType(stat, placementInfo.peerType, module, statement, "placed on")
    placementInfo.modality.subjectivePeerType foreach { checkPeerType(stat, _, module, subjectiveStatement, "subjective to") }

  def normalizePlacedStatements(module: ClassDef): ClassDef =
    val body = module.body map:
      case stat @ ValDef(name, tpt, rhs) =>
        placementType(stat, tpt).fold(stat): placementInfo =>
          checkPlacementType(stat, placementInfo, module)
          ValDef.copy(stat)(name, tpt, cleanPlacementSyntax(placementInfo, rhs)(stat.symbol))

      case stat @ DefDef(name, paramss, tpt, rhs) =>
        placementType(stat, tpt).fold(stat): placementInfo =>
          checkPlacementType(stat, placementInfo, module)
          if !placementInfo.modality.local then
            paramss collectFirst Function.unlift(_.params find { _.symbol.isImplicit }) foreach: param =>
              errorAndCancel("Non-local placed definitions cannot have context parameters.", param.posInUserCode)
          DefDef.copy(stat)(name, paramss, tpt, cleanPlacementSyntax(placementInfo, rhs)(stat.symbol))

      case stat: Term =>
        PlacementInfo(stat.tpe).fold(stat): placementInfo =>
          val (expr, placedExpressionSyntax) = cleanPlacementSyntax(placementInfo, stat)(module.symbol)
          if !placedExpressionSyntax then
            errorAndCancel(s"Placed statements must be enclosed in a placed block: on[${placementInfo.peerType.safeShow}]", stat.posInUserCode.startPosition)
          if placementInfo.modality.subjective then
            errorAndCancel("Placed statements cannot be subjective.", stat.posInUserCode.startPosition)
          if placementInfo.modality.local then
            errorAndCancel("Placed statements cannot be local.", stat.posInUserCode.startPosition)
          checkPlacementType(stat, placementInfo, module)
          expr

      case stat =>
        stat
    end body

    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body)
  end normalizePlacedStatements

  private class NormalizedDefBodyProcessor(transform: Option[((ValDef, Symbol) => ValDef, (Symbol, Term, Term) => (Term, Option[Term]))]) extends TreeMap:
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
            val (rhs, expr) = transform(lambda.symbol, body, erased)
            Block.copy(term)(List(DefDef.copy(lambda)(name, args, tpt, Some(expr.fold(rhs) { appendExpr(block)(rhs, _) }))), closure)
          case _ =>
            body

      // TODO: this "outer" stuff should be copied/duplicated depending on some configuration argument

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

  def transformNormalizedExpression(term: Term, owner: Symbol, transformArg: (ValDef, Symbol) => ValDef, transform: (Symbol, Term, Term) => (Term, Option[Term])) =
    val processor = NormalizedDefBodyProcessor(transform = Some(transformArg, transform))
    processor.transformTerm(term)(owner)

  def extractNormalizedExpression(term: Term, owner: Symbol) =
    val processor = NormalizedDefBodyProcessor(transform = None)
    processor.transformTerm(term)(owner)
end PlacedStatements
