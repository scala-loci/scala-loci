package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.quoted.*

trait PlacedExpressions:
  this: Component with Commons with ErrorReporter with PlacementInfo with PlacementContextTypes =>
  import quotes.reflect.*

  private def checkPlacementTypes(tpe: TypeRepr, pos: Position) =
    val symbol = tpe.typeSymbol
    if symbol == symbols.`language.per` ||  symbol == symbols.`language.on` ||  symbol == symbols.`embedding.on` ||
       symbol == symbols.subjective || symbol == symbols.`language.Local` ||
       !(tpe =:= TypeRepr.of[Nothing]) && (tpe <:< types.context || tpe <:< types.placedValue || tpe <:< types.subjective) then
      errorAndCancel("Illegal multitier construct.", pos)

  private class TypePlacementTypesEraser(pos: Position, checkOnly: Boolean) extends SimpleTypeMap(quotes):
    override def transform(tpe: TypeRepr) =
      val erasedType =
        if !checkOnly then
          val corrected = tpe match
            case AndType(AppliedType(tycon, args), right) if tycon.typeSymbol == symbols.placed && args.last =:= right =>
              symbols.`embedding.on`.typeRef.appliedTo(args.reverse)
            case _ =>
              tpe

          PlacementInfo(corrected).fold(super.transform(corrected)) { placementInfo =>
            if placementInfo.modality.subjective then
              TypeRepr.of[Unit]
            else
              transform(placementInfo.valueType)
          }
        else
          super.transform(tpe)

      checkPlacementTypes(erasedType, pos)

      val termSymbol = erasedType.termSymbol

      def underExpansion(symbol: Symbol): Boolean =
        symbol == Symbol.spliceOwner || symbol.maybeOwner.exists && underExpansion(symbol.maybeOwner)

      if !canceled && termSymbol.exists && (isMultitierModule(termSymbol.owner) || !underExpansion(termSymbol)) then
        val widenedErasedType = erasedType.widen
        if widenedErasedType != erasedType && isMultitierModule(erasedType.termSymbol.owner) then
          TypePlacementTypesEraser(pos, checkOnly = true).transform(widenedErasedType)

      erasedType
  end TypePlacementTypesEraser

  private abstract class AdapatingDefinitionTypeCopier(substitute: (Symbol, Symbol) => Unit, replace: (Symbol, Symbol) => Unit) extends TreeMap:
    def copyAnnotations(from: Symbol, to: Symbol) =
      if from.annotations.nonEmpty then
        SymbolMutator.get.fold(
          errorAndCancel("Annotations not supported for definitions that refer to placement types with current compiler version. Try ascribing their types explicitly.", from.annotations.head.posInUserCode)):
          symbolMutator => from.annotations foreach { symbolMutator.updateAnnotationWithTree(to, _) }

    def adaptedDefinitionType(stat: Definition): TypeRepr

    override def transformTerm(term: Term)(owner: Symbol) = term match
      case Lambda(_, _) =>
        val Block(List(lambda: DefDef), closure: Closure) = term: @unchecked
        val transformedLambda = transformDefDef(lambda)(owner)
        if transformedLambda.symbol != lambda.symbol then
          Block.copy(term)(List(transformedLambda), Closure(Ref(transformedLambda.symbol), closure.tpeOpt))
        else
          Block.copy(term)(List(transformedLambda), closure)
      case _ =>
        super.transformTerm(term)(owner)

    override def transformStatement(stat: Statement)(owner: Symbol) = stat match
      case stat: ValDef => transformValDef(stat)(owner)
      case stat: DefDef => transformDefDef(stat)(owner)
      case _ => super.transformStatement(stat)(owner)

    def transformValDef(stat: ValDef)(owner: Symbol) =
      val symbol = stat.symbol
      val tpt = transformTypeTree(stat.tpt)(symbol)
      val rhs = stat.rhs map { transformTerm(_)(symbol) }
      if canceled then
        stat
      else if tpt == stat.tpt then
        ValDef.copy(stat)(stat.name, tpt, rhs)
      else
        val info = adaptedDefinitionType(stat)
        val privateWithin = if stat.symbol.flags is Flags.Protected then stat.symbol.protectedWithin else stat.symbol.privateWithin
        val symbol = Symbol.newVal(owner, stat.name, info, stat.symbol.flags, privateWithin.fold(Symbol.noSymbol) { _.typeSymbol })
        copyAnnotations(stat.symbol, symbol)
        substitute(stat.symbol, symbol)
        if stat.symbol.owner.isClassDef then replace(stat.symbol, symbol)
        ValDef(symbol, rhs map { _.changeOwner(symbol) })

    def transformDefDef(stat: DefDef)(owner: Symbol) =
      val paramss = stat.paramss mapConserve:
        case TypeParamClause(params) => TypeParamClause(transformSubTrees(params)(owner))
        case TermParamClause(params) => TermParamClause(transformSubTrees(params)(owner))
      val tpt = transformTypeTree(stat.returnTpt)(stat.symbol)
      val rhs = stat.rhs map { transformTerm(_)(stat.symbol) }
      if canceled then
        stat
      else if paramss == stat.paramss && tpt == stat.returnTpt then
        DefDef.copy(stat)(stat.name, paramss, tpt, rhs)
      else
        val info = adaptedDefinitionType(stat)
        val privateWithin = if stat.symbol.flags is Flags.Protected then stat.symbol.protectedWithin else stat.symbol.privateWithin
        val symbol = Symbol.newMethod(owner, stat.name, info, stat.symbol.flags, privateWithin.fold(Symbol.noSymbol) { _.typeSymbol })
        copyAnnotations(stat.symbol, symbol)
        substitute(stat.symbol, symbol)
        if stat.symbol.owner.isClassDef then replace(stat.symbol, symbol)
        DefDef(symbol, paramss => {
          (stat.paramss flatMap { _.params map { _.symbol } }) zip (paramss.flatten map { _.symbol }) foreach { substitute(_, _) }
          rhs map { _.changeOwner(symbol) } })
  end AdapatingDefinitionTypeCopier

  private class ExpressionPlacementTypesEraser(checkOnly: Boolean, substitute: (Symbol, Symbol) => Unit, replace: (Symbol, Symbol) => Unit)
      extends AdapatingDefinitionTypeCopier(substitute, replace):

    def adaptedDefinitionType(stat: Definition) =
      TypePlacementTypesEraser(stat.posInUserCode, checkOnly).transform(stat.symbol.info)

    override def transformTerm(term: Term)(owner: Symbol) = term match
      // check there are no remaining placement lifting conversions
      case PlacementLiftingConversion(expr) =>
        transformTerm(expr)(owner)
        if !canceled then errorAndCancel("Illegal multitier construct.", term.posInUserCode)
        term

      // check access to subjective placed values
      case PlacedValue(_, placementInfo) if !checkOnly =>
        if placementInfo.modality.subjective then
          errorAndCancel("Illegal subjective access.", term.posInUserCode)
        super.transformTerm(term)(owner)

      // check arguments before applied function to improve error messages
      case term @ Apply(_, _) =>
        val args = transformTerms(term.args)(owner)
        val fun = if canceled then term.fun else transformTerm(term.fun)(owner)
        Apply.copy(term)(fun, args)

      // keep direct placed values accesses through the intended language constructs
      case Select(qualifier @ PlacedValue(_, _), name) if !checkOnly && term.symbol.owner == symbols.placed =>
        qualifier match
          case Apply(fun: Select, args) =>
            Select.copy(term)(Apply.copy(qualifier)(Select.copy(fun)(super.transformTerm(fun.qualifier)(owner), fun.name), args), name)
          case _ =>
            Select.copy(term)(transformTerm(qualifier)(owner), name)

      // check other placed value accesses
      case term @ Select(_, name) if !checkOnly =>
        PlacementInfo(term.qualifier.tpe.widenTermRefByName.resultType).fold(super.transformTerm(term)(owner)): placementInfo =>
          val qualifier = transformTerm(term.qualifier)(owner)
          val tpe = if placementInfo.modality.subjective then TypeRepr.of[Unit] else placementInfo.valueType
          if !canceled && !placementInfo.canonical && !(tpe.baseClasses contains term.symbol.owner) then
            errorAndCancel(s"${term.symbol} is not a member of ${tpe.safeShow(Printer.SafeTypeReprShortCode)}", term.posInUserCode)
            term
          else
            Select.copy(term)(qualifier, name)

      // check type of identifiers
      case Ident(_) | Select(_, _) if checkOnly =>
        TypePlacementTypesEraser(term.posInUserCode, checkOnly = true).transform(term.tpe)
        super.transformTerm(term)(owner)

      // check that there are no remaining multitier terms
      case _ =>
        if term.symbol.exists && term.symbol.owner == symbols.placed then
          errorAndCancel("Illegal multitier construct.", term.posInUserCode)
        super.transformTerm(term)(owner)

    override def transformTypeTree(tree: TypeTree)(owner: Symbol) = tree match
      // check inferred type trees and transform them to erase placement types (if possible)
      case Inferred() =>
        if !canceled then
          val eraser = TypePlacementTypesEraser(tree.posInUserCode, checkOnly)
          val tpe = eraser.transform(tree.tpe)
          if checkOnly || canceled || tpe == tree.tpe then tree else TypeTree.of(using tpe.asType)
        else
          tree

      // first check all parts of non-inferred type trees (shallowly)
      // before performing a deeper check of the types (including type widening)
      // to improve error messages, but never transform non-inferred type trees
      case _ =>
        def pos = tree match
          case Applied(tpt, _) => tpt.posInUserCode
          case _ => tree.posInUserCode
        checkPlacementTypes(tree.tpe, pos)
        val transformTree = super.transformTypeTree(tree)(owner)
        if !canceled then
          TypePlacementTypesEraser(transformTree.posInUserCode, checkOnly = true).transform(transformTree.tpe)
        transformTree
  end ExpressionPlacementTypesEraser

  private class ReferenceSubstitutor(substitutions: List[(Symbol, Symbol)], substitute: (Symbol, Symbol) => Unit, replace: (Symbol, Symbol) => Unit)
      extends AdapatingDefinitionTypeCopier(substitute, replace):
    val (from, to) = (substitutions map { (from, to) => from -> (if to.isType then to.typeRef else to.termRef) }).unzip

    def substitution(symbol: Symbol, substitutions: List[(Symbol, Symbol)]): Symbol = substitutions match
      case (from, to) :: tail => if from == symbol then to else substitution(symbol, tail)
      case _ => Symbol.noSymbol

    def adaptedDefinitionType(stat: Definition) =
      stat.symbol.info.substituteTypes(from, to)

    override def transformTerm(term: Term)(owner: Symbol) = term match
      case term: Ident =>
        val symbol = substitution(term.symbol, substitutions)
        if symbol.isTerm then Ref(symbol) else super.transformTerm(term)(owner)
      case term: Select =>
        val symbol = substitution(term.symbol, substitutions)
        if symbol.isTerm then Select(transformTerm(term.qualifier)(owner), symbol) else super.transformTerm(term)(owner)
      case _ =>
        super.transformTerm(term)(owner)

    override def transformTypeTree(tree: TypeTree)(owner: Symbol) = tree match
      case Inferred() =>
        val tpe = tree.tpe.substituteTypes(from, to)
        if tpe == tree.tpe then tree else TypeTree.of(using tpe.asType)
      case tree: TypeIdent =>
        val symbol = substitution(tree.symbol, substitutions)
        if symbol.isType then TypeIdent(symbol) else super.transformTypeTree(tree)(owner)
      case tree: TypeSelect =>
        val symbol = substitution(tree.symbol, substitutions)
        if symbol.isType then TypeSelect(transformTerm(tree.qualifier)(owner), symbol.name) else super.transformTypeTree(tree)(owner)
      case _ =>
        super.transformTypeTree(tree)(owner)
  end ReferenceSubstitutor

  private def substituteReferences(term: Term, owner: Symbol, substitutions: List[(Symbol, Symbol)], replacements: List[(Symbol, Symbol)]) =
    def substituteReferences(term: Term, substitutions: List[(Symbol, Symbol)], replacements: List[(Symbol, Symbol)], substituted: Set[Symbol]): Term =
      if substitutions.nonEmpty then
        var newSubstitutions = List.empty[(Symbol, Symbol)]
        var newReplacements = List.empty[(Symbol, Symbol)]
        val expr = ReferenceSubstitutor(substitutions, newSubstitutions ::= _ -> _, newReplacements ::= _ -> _).transformTerm(term)(owner)
        if newSubstitutions exists { (from, _) => substituted contains from } then
          errorAndCancel(
            s"Illegal cyclic definitions encountered while checking placement types, involving\n  - ${substituted.mkString("\n  - ")}",
            term.posInUserCode)
          term
        else
          substituteReferences(expr, newSubstitutions, replacements ++ newReplacements, substituted ++ (newSubstitutions map { (_, to) => to }))
      else
        if !canceled && replacements.nonEmpty then
          val (headFrom, _) = replacements.head
          SymbolMutator.get.fold(
            errorAndCancel(
              "Type inference not supported with current compiler version for definitions nested in classes/traits/objects " +
              "that refer to placement types. Try ascribing their types explicitly.",
              headFrom.pos getOrElse Position.ofMacroExpansion)):
            symbolMutator => replacements foreach { (from, to) => symbolMutator.replace(from.owner, from, to) }
        term

    substituteReferences(term, substitutions, replacements, Set.empty)
  end substituteReferences

  private val eraserCheckOnly = ExpressionPlacementTypesEraser(checkOnly = true, (_, _) => (), (_, _) => ())

  private def erasePlacementTypesFromBody(term: Term, owner: Symbol) =
    var substitutions = List.empty[(Symbol, Symbol)]
    var replacements = List.empty[(Symbol, Symbol)]
    val eraser = ExpressionPlacementTypesEraser(checkOnly = false, substitutions ::= _ -> _, replacements ::= _ -> _)
    val expr = transformNormalizedExpression(term, owner, eraserCheckOnly.transformValDef(_)(_), (term, _, symbol) => eraser.transformTerm(term)(symbol))
    if canceled then term else substituteReferences(expr, owner, substitutions, replacements)

  private def checkPlacementTypesInArguments(paramss: List[ParamClause], owner: Symbol) =
    paramss foreach { _.params foreach { eraserCheckOnly.transformStatement(_)(owner) } }

  private def checkPlacementTypesInResult(tpt: TypeTree, owner: Symbol) = tpt match
    case Inferred() =>
      val eraser = TypePlacementTypesEraser(tpt.posInUserCode, checkOnly = true)
      PlacementInfo(tpt.tpe).fold(eraser.transform(tpt.tpe)) { placementInfo =>
        eraser.transform(placementInfo.valueType)
        eraser.transform(placementInfo.peerType)
        placementInfo.modality.subjectivePeerType foreach eraser.transform
      }
    case Applied(on: TypeIdent, List(value, peer)) if on.symbol == symbols.`language.on` => value match
      case Applied(per: TypeIdent, List(value, subjective)) if per.symbol == symbols.`language.per` =>
        eraserCheckOnly.transformTree(subjective)(owner)
        eraserCheckOnly.transformTree(value)(owner)
        eraserCheckOnly.transformTree(peer)(owner)
      case Applied(local: TypeIdent, List(value)) if local.symbol == symbols.`language.Local` =>
        eraserCheckOnly.transformTree(value)(owner)
        eraserCheckOnly.transformTree(peer)(owner)
      case _ =>
        eraserCheckOnly.transformTree(value)(owner)
        eraserCheckOnly.transformTree(peer)(owner)
    case _ =>
      eraserCheckOnly.transformTree(tpt)(owner)

  def erasePlacementTypesFromExpressions(module: ClassDef): ClassDef =
    val body = module.body map:
      case PlacedStatement(stat @ ValDef(name, tpt, rhs)) =>
        checkPlacementTypesInResult(tpt, stat.symbol)
        ValDef.copy(stat)(name, tpt, rhs map { erasePlacementTypesFromBody(_, stat.symbol) })
      case PlacedStatement(stat @ DefDef(name, paramss, tpt, rhs)) =>
        checkPlacementTypesInArguments(paramss, stat.symbol)
        checkPlacementTypesInResult(tpt, stat.symbol)
        DefDef.copy(stat)(name, paramss, tpt, rhs map { erasePlacementTypesFromBody(_, stat.symbol) })
      case PlacedStatement(stat: Term) =>
        erasePlacementTypesFromBody(stat, module.symbol)
      case stat =>
        eraserCheckOnly.transformStatement(stat)(module.symbol)
        stat

    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body)
  end erasePlacementTypesFromExpressions
end PlacedExpressions
