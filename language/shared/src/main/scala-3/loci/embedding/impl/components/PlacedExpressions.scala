package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental

@experimental
trait PlacedExpressions:
  this: Component & Commons & ErrorReporter & Placements & PlacedTransformations & PlacedStatements =>
  import quotes.reflect.*

  private object PlacementLiftingConversion:
    def unapply(term: Term): Option[Term] = term match
      case Inlined(Some(call), List(conversion: ValDef), erased @ MaybeTyped(Apply(_, List(rhs))))
        if call.symbol == symbols.placed.companionModule.moduleClass &&
           !(conversion.tpt.tpe =:= TypeRepr.of[Nothing]) && conversion.tpt.tpe <:< types.conversion &&
           !(erased.tpe =:= TypeRepr.of[Nothing]) && erased.tpe <:< types.placed =>
        rhs match
          case MaybeTyped(Repeated(List(Inlined(_, List(), rhs)), _)) => Some(rhs)
          case MaybeTyped(Repeated(List(rhs), _)) => Some(rhs)
          case _ => Some(rhs)
      case Inlined(Some(call), List(conversion: ValDef, ValDef(_, _, Some(rhs))), erased @ MaybeTyped(Apply(_, List(_))))
        if call.symbol == symbols.placed.companionModule.moduleClass &&
           !(conversion.tpt.tpe =:= TypeRepr.of[Nothing]) && conversion.tpt.tpe <:< types.conversion &&
           !(erased.tpe =:= TypeRepr.of[Nothing]) && erased.tpe <:< types.placed =>
        rhs match
          case Inlined(_, List(), rhs) => Some(rhs)
          case _ => Some(rhs)
      case Apply(Select(conversion, _), List(rhs))
        if conversion.symbol.exists && conversion.symbol.owner == symbols.placed.companionModule.moduleClass &&
           !(conversion.tpe =:= TypeRepr.of[Nothing]) && conversion.tpe <:< types.conversion =>
        rhs match
          case Inlined(_, List(), rhs) => Some(rhs)
          case _ => Some(rhs)
      case _ =>
        None

  private object MultitierConstructFragment:
    def unapply(term: Term): Boolean = term match
      case Inlined(Some(call), _, _)
          if call.symbol.hasAncestor(symbols.on, symbols.on.companionModule.moduleClass, symbols.placed.companionModule.moduleClass) =>
        true
      case Apply(Select(conversion, _), List(_))
          if conversion.symbol.hasAncestor(symbols.on, symbols.on.companionModule.moduleClass, symbols.placed.companionModule.moduleClass) =>
        true
      case _ =>
        (term.symbol == symbols.erased || term.symbol == symbols.erasedArgs) &&
        !(term.tpe =:= TypeRepr.of[Nothing]) && term.tpe <:< types.placed

  private def peerContext(owner: Symbol): Option[TypeRepr] =
    if owner.exists then
      PlacementInfo(owner.info.widenTermRefByName.resultType).fold(peerContext(owner.maybeOwner)): placementInfo =>
        Some(placementInfo.peerType)
    else
      None

  private def checkPlacementTypes(tpe: TypeRepr, pos: Position, message: String) =
    val symbol = tpe.typeSymbol
    if symbol == symbols.`language.per` || symbol == symbols.`language.on` ||
       symbol == symbols.`embedding.on` || symbol == symbols.`embedding.of` ||
       symbol == symbols.subjective || symbol == symbols.`language.Local` ||
       !(tpe =:= TypeRepr.of[Nothing]) && (tpe <:< types.context || tpe <:< types.placedValue || tpe <:< types.subjective) then
      errorAndCancel(message, pos)

  private class TypePlacementTypesEraser(pos: Position, localPeer: Option[TypeRepr], checkOnly: Boolean) extends TypeMap(quotes):
    override def transform(tpe: TypeRepr) =
      val erasedType =
        if !checkOnly then
          val corrected = tpe match
            case AndType(AppliedType(tycon, args), right) if tycon.typeSymbol == symbols.placed && args.last =:= right =>
              symbols.`embedding.on`.typeRef.appliedTo(args.reverse)
            case _ =>
              tpe
          PlacementInfo(corrected).fold(super.transform(corrected)): placementInfo =>
            if placementInfo.modality.subjective || !(localPeer forall { _ =:= placementInfo.peerType }) then
              TypeRepr.of[Unit]
            else
              transform(placementInfo.valueType)
        else
          super.transform(tpe)

      checkPlacementTypes(erasedType, pos, "Illegal use of value with placement type.")

      val termSymbol = erasedType.termSymbol

      def underExpansion(symbol: Symbol): Boolean =
        symbol == Symbol.spliceOwner || symbol.maybeOwner.exists && underExpansion(symbol.maybeOwner)

      if !canceled && termSymbol.exists && (isMultitierModule(termSymbol.owner) || !underExpansion(termSymbol)) then
        val widenedErasedType = erasedType.widen
        if widenedErasedType != erasedType && isMultitierModule(erasedType.termSymbol.owner) then
          TypePlacementTypesEraser(pos, localPeer, checkOnly = true).transform(widenedErasedType)

      erasedType
  end TypePlacementTypesEraser

  private abstract class AdapatingDefinitionTypeCopier(substitute: (Symbol, Symbol) => Unit, replace: (Symbol, Symbol) => Unit) extends SafeTreeMap(quotes):
    def copyAnnotations(from: Symbol, to: Symbol) =
      if from.annotations.nonEmpty then
        SymbolMutator.get.fold(
          errorAndCancel("Annotations not supported for definitions that refer to placement types with current compiler version. Try ascribing their types explicitly.", from.annotations.head.posInUserCode)):
          symbolMutator => from.annotations foreach { symbolMutator.updateAnnotationWithTree(to, _) }

    def adaptedDefinitionType(stat: ValDef | DefDef): TypeRepr

    override def transformTerm(term: Term)(owner: Symbol) = term match
      case Lambda(_, _) =>
        val Block(List(lambda: DefDef), closure: Closure) = term: @unchecked
        val transformedLambda = transformDefDef(lambda)(owner)
        if transformedLambda.symbol != lambda.symbol then
          Block.copy(term)(List(transformedLambda), transformTerm(Closure(Ref(transformedLambda.symbol), closure.tpeOpt))(owner))
        else
          Block.copy(term)(List(transformedLambda), transformTerm(closure)(owner))
      case _ =>
        super.transformTerm(term)(owner)

    override def transformStatement(stat: Statement)(owner: Symbol) = stat match
      case stat: ValDef => transformValDef(stat)(owner)
      case stat: DefDef => transformDefDef(stat)(owner)
      case _ => super.transformStatement(stat)(owner)

    def transformValDef(stat: ValDef)(owner: Symbol) =
      val tpt = transformTypeTree(stat.tpt)(stat.symbol)
      if canceled then
        stat
      else if tpt == stat.tpt then
        ValDef.copy(stat)(stat.name, tpt, stat.rhs map { transformTerm(_)(stat.symbol) })
      else
        val info = adaptedDefinitionType(stat)
        val privateWithin = if stat.symbol.flags is Flags.Protected then stat.symbol.protectedWithin else stat.symbol.privateWithin
        val symbol = newVal(owner, stat.name, info, stat.symbol.flags, privateWithin.fold(Symbol.noSymbol) { _.typeSymbol })
        copyAnnotations(stat.symbol, symbol)
        substitute(stat.symbol, symbol)
        if stat.symbol.owner.isClassDef then replace(stat.symbol, symbol)
        ValDef(symbol, stat.rhs map { rhs => transformTerm(rhs.changeOwner(symbol))(symbol) })

    def transformDefDef(stat: DefDef)(owner: Symbol) =
      val paramss = stat.paramss mapConserve:
        case TypeParamClause(params) => TypeParamClause(transformSubTrees(params)(stat.symbol))
        case TermParamClause(params) => TermParamClause(transformSubTrees(params)(stat.symbol))
      val tpt = transformTypeTree(stat.returnTpt)(stat.symbol)
      if canceled then
        stat
      else if paramss == stat.paramss && tpt == stat.returnTpt then
        DefDef.copy(stat)(stat.name, paramss, tpt, stat.rhs map { transformTerm(_)(stat.symbol) })
      else
        val info = adaptedDefinitionType(stat)
        val privateWithin = if stat.symbol.flags is Flags.Protected then stat.symbol.protectedWithin else stat.symbol.privateWithin
        val symbol = newMethod(owner, stat.name, info, stat.symbol.flags, privateWithin.fold(Symbol.noSymbol) { _.typeSymbol })
        copyAnnotations(stat.symbol, symbol)
        substitute(stat.symbol, symbol)
        if stat.symbol.owner.isClassDef then replace(stat.symbol, symbol)
        DefDef(symbol, paramss =>
          (stat.paramss flatMap { _.params map { _.symbol } }) zip (paramss.flatten map { _.symbol }) foreach { substitute(_, _) }
          stat.rhs map { rhs => transformTerm(rhs.changeOwner(symbol))(symbol) })
  end AdapatingDefinitionTypeCopier

  private class ExpressionPlacementTypesEraser(checkOnly: Boolean, substitute: (Symbol, Symbol) => Unit, replace: (Symbol, Symbol) => Unit)
      extends AdapatingDefinitionTypeCopier(substitute, replace):

    def adaptedDefinitionType(stat: ValDef | DefDef) =
      TypePlacementTypesEraser(stat.posInUserCode, peerContext(stat.symbol), checkOnly).transform(stat.symbol.info)

    override def transformTerm(term: Term)(owner: Symbol) = term match
      // erase placement lifting conversions
      case PlacementLiftingConversion(expr) if !checkOnly =>
        transformTerm(expr.changeOwner(owner))(owner)

      // check there are no remaining multitier language constructs
      case MultitierConstructFragment() =>
        errorAndCancel("Multitier construct should have been processed by macro expansion but was not.", term.posInUserCode)
        term

      // check access to subjective placed values
      case PlacedValueReference(_, placementInfo) if !checkOnly =>
        if placementInfo.modality.subjective then
          errorAndCancel("Illegal subjective access.", term.posInUserCode)

        peerContext(owner) foreach: localPeer =>
          if !(localPeer =:= placementInfo.peerType) then
            errorAndCancel(
              s"Access to value on peer ${placementInfo.peerType.safeShow(Printer.SafeTypeReprShortCode)} not allowed " +
              s"from peer ${localPeer.safeShow(Printer.SafeTypeReprShortCode)}",
              term.posInUserCode)

        super.transformTerm(term)(owner)

      // keep direct placed values accesses through the intended language constructs that expect placed values
      // check arguments before applied function to improve error messages
      case term @ Apply(_, _) =>
        def clearTypeApplications(term: Term): Term = term match
          case Apply(fun, args) =>
            Apply.copy(term)(clearTypeApplications(fun), args)
          case TypeApply(fun, args) => fun.tpe.widenTermRefByName match
            case tpe @ PolyType(_, paramTypes, _) if paramTypes.sizeIs == args.size =>
              TypeApply.copy(term)(clearTypeApplications(fun), (0 until paramTypes.size map { i => TypeTree.of(using tpe.param(i).asType) }).toList)
            case _ =>
              TypeApply.copy(term)(clearTypeApplications(fun), args)
          case _ =>
            term

        val args = clearTypeApplications(term.fun).tpe match
          case MethodType(_, paramTypes, _) =>
            paramTypes zip term.args map: (tpe, arg) =>
              if !(tpe =:= TypeRepr.of[Nothing]) && tpe <:< types.placedValue then
                arg
              else
                transformTerm(arg)(owner)
          case _ =>
            transformTerms(term.args)(owner)

        val fun = if canceled then term.fun else transformTerm(term.fun)(owner)

        Apply.copy(term)(fun, args)

      // keep direct placed values accesses through the intended language constructs defined on placed values
      case Select(qualifier @ PlacedValueReference(_, _), name) if !checkOnly && term.symbol.owner == symbols.placed =>
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
        TypePlacementTypesEraser(term.posInUserCode, peerContext(owner), checkOnly = true).transform(term.tpe)
        super.transformTerm(term)(owner)

      // check that there are no remaining multitier terms
      case _ =>
        if term.symbol.exists && term.symbol.owner == symbols.placed then
          errorAndCancel("Illegal use of multitier construct.", term.posInUserCode)
        super.transformTerm(term)(owner)

    override def transformTypeTree(tree: TypeTree)(owner: Symbol) = tree match
      // check inferred type trees and transform them to erase placement types (if possible)
      case Inferred() =>
        if !canceled then
          val eraser = TypePlacementTypesEraser(tree.posInUserCode, peerContext(owner), checkOnly)
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
        checkPlacementTypes(tree.tpe, pos, "Illegal use of multitier construct.")
        val transformTree = super.transformTypeTree(tree)(owner)
        if !canceled then
          TypePlacementTypesEraser(transformTree.posInUserCode, peerContext(owner), checkOnly = true).transform(transformTree.tpe)
        transformTree
  end ExpressionPlacementTypesEraser

  private object typesInClosuresChecker extends TreeTraverser:
    override def traverseTree(tree: Tree)(owner: Symbol) = tree match
      case Lambda(_, _) =>
        val Block(_, Closure(_, tpe)) = tree: @unchecked
        tpe foreach { TypePlacementTypesEraser(tree.posInUserCode, peerContext(owner), checkOnly = true).transform(_) }
        super.traverseTree(tree)(owner)
      case _ =>
        super.traverseTree(tree)(owner)

  private def eraseSubjectiveTypesInClosures(term: Term): Term = term match
    case Lambda(_, _) =>
      val Block(stats, closure @ Closure(meth, tpe)) = term: @unchecked
      if tpe exists { _.widenDealias.typeSymbol == symbols.subjective } then
        Block.copy(term)(stats, Closure.copy(closure)(meth, None))
      else
        term
    case Block(stats, expr) =>
      Block.copy(term)(stats, eraseSubjectiveTypesInClosures(expr))
    case _ =>
      term

  private class ReferenceSubstitutor(substitutions: List[(Symbol, Symbol)], substitute: (Symbol, Symbol) => Unit, replace: (Symbol, Symbol) => Unit)
      extends AdapatingDefinitionTypeCopier(substitute, replace):
    val (from, to) = (substitutions map { (from, to) => from -> (if to.isType then to.typeRef else to.termRef) }).unzip

    def substitution(symbol: Symbol, substitutions: List[(Symbol, Symbol)]): Symbol = substitutions match
      case (from, to) :: tail => if from == symbol then to else substitution(symbol, tail)
      case _ => Symbol.noSymbol

    def adaptedDefinitionType(stat: ValDef | DefDef) =
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
    val expr = transformPlacedBody(term): (symbol, body, expr) =>
      val term = if canceled then body else eraseSubjectiveTypesInClosures(eraser.transformTerm(body)(symbol))
      if !canceled then typesInClosuresChecker.traverseTree(term)(owner)
      term -> Some(expr)
    if canceled then term else substituteReferences(expr, owner, substitutions, replacements)

  private def checkPlacementTypesInArguments(paramss: List[ParamClause], owner: Symbol) =
    paramss foreach { _.params foreach { eraserCheckOnly.transformStatement(_)(owner) } }

  private def checkPlacementTypesInResult(tpt: TypeTree, owner: Symbol) =
    def skipToCode(content: String, pos: Int): Int =
      var level = 0
      var i = pos
      val length = content.length
      while i < length do
        if level == -1 then
          if content(i) == '\r' || content(i) == '\n' then level = 0
        else if i + 1 < length && content(i) == '/' && content(i + 1) == '*' then
          level += 1
        else if i + 1 < length && content(i) == '*' && content(i + 1) == '/' then
          i += 1
          if level > 0 then level -= 1
        else if level == 0 then
          if i + 1 < length && content(i) == '/' && content(i + 1) == '/' then level = -1
          else if !Character.isWhitespace(content(i)) then return i
        i += 1
      i
    end skipToCode

    def notationWarning() =
      PlacementInfo(tpt.tpe) foreach: placementInfo =>
        report.warning(
          s"Discouraged placement type notation. Expected type notation: ${placementInfo.showCanonical}" +
          s"${System.lineSeparator}Placement types are imported by: import loci.language.*", tpt.posInUserCode)

    def checkNotation(tree: Tree, infix: Boolean) = tree match
      case _: TypeIdent =>
        if infix then
          tree.pos.sourceFile.content foreach: content =>
            val pos = skipToCode(content, tree.pos.end)
            if pos < content.length && content(pos) == '[' then
              notationWarning()
      case _ =>
        notationWarning()

    def checkValueType(value: TypeTree) = value match
      case Applied(per @ (_: TypeIdent | _: TypeSelect), List(value, subjective)) if per.symbol == symbols.`language.per` =>
        eraserCheckOnly.transformTree(subjective)(owner)
        eraserCheckOnly.transformTree(value)(owner)
        checkNotation(per, infix = true)
      case Applied(local @ (_: TypeIdent | _: TypeSelect), List(value)) if local.symbol == symbols.`language.Local` =>
        eraserCheckOnly.transformTree(value)(owner)
        checkNotation(local, infix = false)
      case _ =>
        eraserCheckOnly.transformTree(value)(owner)

    tpt match
      case Inferred() =>
        val eraser = TypePlacementTypesEraser(tpt.posInUserCode, peerContext(owner), checkOnly = true)
        PlacementInfo(tpt.tpe).fold(eraser.transform(tpt.tpe)) { placementInfo =>
          eraser.transform(placementInfo.valueType)
          eraser.transform(placementInfo.peerType)
          placementInfo.modality.subjectivePeerType foreach eraser.transform
        }

      case Applied(on @ (_: TypeIdent | _: TypeSelect), List(value: TypeTree, peer: TypeTree))
          if on.symbol == symbols.`language.on` =>
        eraserCheckOnly.transformTree(peer)(owner)
        checkNotation(on, infix = true)

        if value.tpe <:< TypeRepr.of[Nothing] then
          value match
            case Applied(of, _) if of.symbol == symbols.`embedding.of` =>
            case _ => errorAndCancel("Illegal use of Nothing for placed value.", value.posInUserCode)

        value match
          case Applied(of: Inferred, List(ofValue: TypeTree, ofPeer: TypeTree))
              if of.symbol == symbols.`embedding.of` =>
            eraserCheckOnly.transformTree(ofPeer)(owner)
            checkValueType(ofValue)
            if !(peer.tpe =:= ofPeer.tpe) then
              errorAndCancel("Illegal use of Nothing for placed value.", value.posInUserCode)
          case _ =>
            checkValueType(value)

      case _ =>
        eraserCheckOnly.transformTree(tpt)(owner)
  end checkPlacementTypesInResult

  def eraseMultitierConstructs(module: ClassDef): ClassDef =
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
      case stat @ DefDef(_, List(TermParamClause(List(_))), tpt, _)
          if tpt.tpe.typeSymbol == defn.UnitClass && stat.symbol.isFieldAccessor =>
        stat
      case stat =>
        eraserCheckOnly.transformStatement(stat)(module.symbol)
        stat

    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body)
  end eraseMultitierConstructs
end PlacedExpressions
