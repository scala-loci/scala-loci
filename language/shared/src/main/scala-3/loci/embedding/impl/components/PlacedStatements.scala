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

  private object PlacementCallBindingArtifact:
    def unapply(term: Term): Option[(List[Definition], Term)] = term match
      case Inlined(Some(call), bindings, body)
          if call.symbol.hasAncestor(symbols.on, symbols.on.companionModule.moduleClass) =>
        Some(bindings, body)
      case _ =>
        None

  private object PlacementCallContextEvidenceArtifact:
    def unapply(term: Term): Option[(Definition, Term)] = term match
      case Inlined(_, List(), block @ Block((evidence: ValDef) :: statements, expr))
          if !(evidence.tpt.tpe =:= TypeRepr.of[Nothing]) && evidence.tpt.tpe <:< types.context =>
        if statements.nonEmpty then
          Some(evidence, Block.copy(block)(statements, expr))
        else
          Some(evidence, expr)
      case _ =>
        None

  private object PlacementCallArtifact:
    def unapply(term: Term): Option[(List[Definition], Term)] = term match
      case PlacementCallBindingArtifact(bindings, expr) => Some(bindings, expr)
      case PlacementCallContextEvidenceArtifact(binding, expr) => Some(List(binding), expr)
      case _ => None

  private object PlacementErasedArtifact:
    def unapply(term: Term): Option[Term] = term match
      case Block(List(statement @ Inlined(_, _, _)), erased: Typed)
          if erased.symbol == symbols.erased || erased.symbol == symbols.erasedArgs =>
        Some(statement)
      case _ =>
        None

  private object PlacedExpresion:
    def unapply(term: Term): Some[(List[Definition], Term)] = Term.tryBetaReduce(term) match
      case PlacementErasedArtifact(PlacementCallArtifact(bindings, expr)) => Term.tryBetaReduce(expr) match
        case PlacedExpresion(nestedBindings, expr) => Some((bindings ++ nestedBindings) -> expr)
      case PlacementCallArtifact(bindings, expr) => Term.tryBetaReduce(expr) match
        case PlacedExpresion(nestedBindings, expr) => Some((bindings ++ nestedBindings) -> expr)
      case term @ Apply(select @ Select(PlacedExpresion(bindings, expr), names.apply), List(arg)) =>
        Term.betaReduce(expr.select(select.symbol).appliedTo(arg)) match
          case Some(PlacedExpresion(nestedBindings, expr)) => Some((bindings ++ nestedBindings) -> expr)
          case _ => Some(List.empty -> term)
      case expr =>
        Some(List.empty -> expr)

  private def bindingsForPlacedConstruct(bindings: List[Definition]) =
    bindings exists: binding =>
      val info = binding.symbol.info
      !(info =:= TypeRepr.of[Nothing]) && info <:< types.contextResolutionWithFallback

  extension (termModule: TermModule)
    private inline def tryBetaReduce(expr: Term) =
      Term.betaReduce(expr) getOrElse expr

  private def cleanPlacementExpression(placementInfo: PlacementInfo, term: Term) =
    val PlacedExpresion(bindings, expr) = term
    val erasedContext = Typed(
      Ref(symbols.erased),
      TypeTree.of(using symbols.`embedding.on`.typeRef.appliedTo(placementInfo.canonicalType.typeArgs).asType))
    bindings -> Block(List(expr), erasedContext)

  private def cleanPlacementExpressionOrClosure(placementInfo: PlacementInfo, expr: Term) =
    expr match
      case block @ Lambda(List(arg), expr)
        if !(arg.tpt.tpe =:= TypeRepr.of[Nothing]) &&
           arg.tpt.tpe <:< types.context &&
           arg.symbol.isImplicit =>
        val Block(List(lambda: DefDef), closure) = block: @unchecked
        val (bindings, body) = cleanPlacementExpression(placementInfo, lambda.rhs.get)
        bindings ->
          Block.copy(block)(
            List(DefDef.copy(lambda)(lambda.name, lambda.paramss, lambda.returnTpt, Some(body.changeOwner(lambda.symbol)))),
            closure)
      case _ =>
        val (bindings, body) = cleanPlacementExpression(placementInfo, expr)
        bindings.headOption.fold(bindings -> body): binding =>
          val peer = placementInfo.peerType.asPackedValueType
          val placement = symbols.`embedding.on`.typeRef.appliedTo(placementInfo.canonicalType.typeArgs).asPackedValueType
          val tpe = contextMethodType[Placement.Context[peer.Type], placement.Type]
          val block @ Block(List(lambda: DefDef), closure @ Closure(meth, _)) =
            Lambda(binding.symbol.owner, tpe, (symbol, _) => body.changeOwner(symbol)): @unchecked
          bindings ->
            Block.copy(block)(List(lambda), Closure.copy(closure)(meth, Some(placementInfo.canonicalType)))

  private object contextVariableCleaner extends SafeTreeMap(quotes):
    private def fallbackIfContextType(term: Term) = term.tpe.asType match
      case '[ Nothing ] | '[ Null ] => term
      case '[ Placement.Context[p] ] => '{ Placement.Context.fallback[p] }.asTerm
      case _ => term
    override def transformTerm(term: Term)(owner: Symbol) = term match
      case Ident(_) => fallbackIfContextType(term)
      case _ if symbols.context.methodMember(term.symbol.name) contains term.symbol => fallbackIfContextType(term)
      case _ => super.transformTerm(term)(owner)

  private def cleanPlacementSyntax(placementInfo: PlacementInfo, rhs: Term)(owner: Symbol): (List[Definition], Term) =
    val (bindings, expr) = cleanPlacementExpressionOrClosure(placementInfo, rhs)
    bindings -> contextVariableCleaner.transformTerm(expr)(owner)

  private def cleanPlacementSyntax(placementInfo: PlacementInfo, rhs: Option[Term])(owner: Symbol): (List[Definition], Option[Term]) =
    rhs.fold(List.empty -> None): rhs =>
      val (bindings, expr) = cleanPlacementSyntax(placementInfo, rhs)(owner)
      bindings -> Some(expr)

  private def cleanSpuriousPlacementSyntax(rhs: Option[Term]): Option[Term] =
    rhs map:
      case PlacedExpresion(_, expr) => expr

  private def cleanSpuriousPlacementSyntax(stat: ValDef | DefDef): ValDef | DefDef =
    stat match
      case ValDef(name, tpt, rhs) => ValDef.copy(stat)(name, tpt, cleanSpuriousPlacementSyntax(rhs))
      case DefDef(name, paramss, tpt, rhs) => DefDef.copy(stat)(name, paramss, tpt, cleanSpuriousPlacementSyntax(rhs))

  private def placementType(stat: ValDef | DefDef, tpt: TypeTree) =
    PlacementInfo(stat.symbol.info.resultType) filter: placementInfo =>
      def pos = tpt match
        case Inferred() => stat.posInUserCode.startPosition
        case _ => tpt.posInUserCode
      if placementInfo.valueType.isContextFunctionType then
        errorAndCancel(s"Placed type cannot be a context function type: ${placementInfo.valueType.safeShow}", pos)
      else if !placementInfo.canonical then
        val message = tpt match
          case Inferred() => "Placement type could not be inferred. Explicit type ascription required."
          case _ => "Invalid placement type."
        errorAndCancel(
          s"$message Expected type: ${placementInfo.showCanonical}" +
          s"${System.lineSeparator}Placement types are imported by: import loci.language.*", pos)
      placementInfo.canonical

  private def checkPeerType(stat: Statement, peerType: TypeRepr, module: ClassDef, statement: String, relation: String): Unit =
    if PeerInfo(peerType).isEmpty then
      errorAndCancel(
        s"$statement must be $relation a peer type but is $relation ${peerType.safeShow}",
        stat.posInUserCode.startPosition)
    if peerType.typeSymbol != defn.AnyClass && !(peerType =:= This(module.symbol).tpe.select(peerType.typeSymbol)) then
      errorAndCancel(
        s"$statement must be $relation a peer of module ${module.symbol.name} " +
        s"but is $relation peer ${peerType.safeShow}",
        stat.posInUserCode.startPosition)

  private def checkPlacementType(stat: Statement, bindings: List[Definition], placementInfo: PlacementInfo, module: ClassDef): Unit =
    val (statement, subjectiveStatement) = stat match
      case _: ValDef | _: DefDef => ("Placed definition", "Subjective placed definition")
      case _ => ("Placed statement", "Subjective placed statement")
    checkPeerType(stat, placementInfo.peerType, module, statement, "placed on")
    placementInfo.modality.subjectivePeerType foreach { checkPeerType(stat, _, module, subjectiveStatement, "subjective to") }

    val (pos, inferred) = stat match
      case ValDef(_, Inferred(), _) | DefDef(_, _, Inferred(), _) => (stat.posInUserCode.startPosition, true)
      case ValDef(_, tpt, _) => (tpt.posInUserCode, false)
      case DefDef(_, _, tpt, _) => (tpt.posInUserCode, false)
      case _ => (stat.posInUserCode.startPosition, false)

    if inferred && (bindings.isEmpty || bindingsForPlacedConstruct(bindings)) then
      errorAndCancel(s"Placed expressions without type ascription must be enclosed in a placed block: on[${placementInfo.peerType.safeShow}]", pos)

    object singletonTypeChecker extends TypeMap(quotes):
      override def transform(tpe: TypeRepr) = tpe match
        case _: TermRef if tpe.termSymbol hasAncestor isMultitierModule =>
          errorAndCancel("Singleton types for values of multitier modules not supported", pos)
          tpe
        case _: NamedType =>
          tpe
        case _ =>
          super.transform(tpe)

    singletonTypeChecker.transform(placementInfo.valueType)
  end checkPlacementType

  def normalizePlacedStatements(module: ClassDef): ClassDef =
    val body = module.body map:
      case stat @ ValDef(name, tpt, rhs) =>
        placementType(stat, tpt).fold(cleanSpuriousPlacementSyntax(stat)): placementInfo =>
          val (bindings, expr) = cleanPlacementSyntax(placementInfo, rhs)(stat.symbol)
          checkPlacementType(stat, bindings, placementInfo, module)
          ValDef.copy(stat)(name, tpt, expr)

      case stat @ DefDef(name, paramss, tpt, rhs) =>
        placementType(stat, tpt).fold(cleanSpuriousPlacementSyntax(stat)): placementInfo =>
          val (bindings, expr) = cleanPlacementSyntax(placementInfo, rhs)(stat.symbol)
          checkPlacementType(stat, bindings, placementInfo, module)
          if !placementInfo.modality.local then
            paramss collectFirst Function.unlift(_.params find { _.symbol.isImplicit }) foreach: param =>
              errorAndCancel("Non-local placed definitions cannot have context parameters.", param.posInUserCode)
          DefDef.copy(stat)(name, paramss, tpt, expr)

      case stat: Term =>
        PlacementInfo(stat.tpe).fold(stat): placementInfo =>
          val (bindings, expr) = cleanPlacementSyntax(placementInfo, stat)(module.symbol)
          if bindings.isEmpty || bindingsForPlacedConstruct(bindings) then
            errorAndCancel(s"Placed statements must be enclosed in a placed block: on[${placementInfo.peerType.safeShow}]", stat.posInUserCode.startPosition)
          if placementInfo.modality.subjective then
            errorAndCancel("Placed statements cannot be subjective.", stat.posInUserCode.startPosition)
          if placementInfo.modality.local then
            errorAndCancel("Placed statements cannot be local.", stat.posInUserCode.startPosition)
          checkPlacementType(stat, bindings, placementInfo, module)
          expr

      case stat =>
        stat
    end body

    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body)
  end normalizePlacedStatements
end PlacedStatements
