package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.quoted.*

trait PlacedExpressions:
  this: Component with Commons with ErrorReporter with PlacementInfo with PlacementContextTypes =>
  import quotes.reflect.*

  def checkPlacementTypes(tpe: TypeRepr, pos: Position) =
    val symbol = tpe.typeSymbol
    if symbol == symbols.`language.per` ||  symbol == symbols.`language.on` ||  symbol == symbols.`embedding.on` ||
       symbol == symbols.`Placed.on` ||  symbol == symbols.`Placed.Subjective.on` || symbol == symbols.subjective ||
       symbol == symbols.`language.Local` ||
       !(tpe =:= TypeRepr.of[Nothing]) && (tpe <:< types.context || tpe <:< types.placedValue || tpe <:< types.subjective) then
      errorAndCancel("Illegal multitier construct.", pos)

  private class TypePlacementTypesEraser(pos: Position, checkOnly: Boolean) extends SimpleTypeMap(quotes):
    override def transform(tpe: TypeRepr) =
      val erasedType =
        if !checkOnly then
          val corrected = tpe match
            case AndType(AppliedType(tycon, args), right) if tycon.typeSymbol == symbols.placed && args.last =:= right =>
              symbols.`Placed.on`.typeRef.appliedTo(args.reverse)
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

      if !canceled then
        val widenedErasedType = erasedType.widen
        if widenedErasedType != erasedType then
          checkPlacementTypes(widenedErasedType, pos)

      erasedType
  end TypePlacementTypesEraser

  private class ExpressionPlacementTypesEraser(checkOnly: Boolean) extends TreeMap:
    def copyAnnotations(from: Symbol, to: Symbol) =
      if from.annotations.nonEmpty then
        SymbolMutator.get.fold(
          errorAndCancel("Annotations not supported for definitions that refer to placement types. Try ascribing their types explicitly.", from.annotations.head.pos)) {
          symbolMutator => from.annotations foreach {
            symbolMutator.updateAnnotationWithTree(to, _)
          }
        }

    override def transformTerm(term: Term)(owner: Symbol) = term match
      case Inlined(Some(call), (conversion: ValDef) :: _, Block(List(DefDef(names.body, List(), _, _)), erased: Typed))
        if call.symbol == symbols.placed.companionModule.moduleClass &&
           !(conversion.tpt.tpe =:= TypeRepr.of[Nothing]) && conversion.tpt.tpe <:< types.conversion &&
           !(erased.tpe =:= TypeRepr.of[Nothing]) && erased.tpe <:< types.placed =>
        if !canceled then
          errorAndCancel("Illegal subjective access.", term.pos)
        term

      case Block(statements, expr) =>
        val (transformedStats, transformedSymbols) = (statements map {
          case stat: Definition =>
            val symbol = stat.symbol
            val info = symbol.info
            val transformedStat = transformDefinition(stat)(owner)
            transformedStat -> Option.when(symbol != transformedStat.symbol || info != transformedStat.symbol.info)(stat.symbol -> transformedStat.symbol)
          case stat =>
            transformStatement(stat)(owner) -> None
        }).unzip
        val transformedExpr = transformTerm(expr)(owner)
        if statements != transformedStats || expr != transformedExpr then
          transformedSymbols.flatten.foldLeft(Block.copy(term)(transformedStats, transformedExpr)) { case (expr, (from, to)) =>
            changeRefs(from, to, owner, expr) match
              case expr: Block => expr
              case _ => expr
          }
        else
          term

      case Select(qualifier @ PlacedValue(_, _), name) if term.symbol.owner == symbols.placed =>
        Select.copy(term)(transformTerm(qualifier)(owner), name)

//      case term: Apply =>
//        val s = term.show + " " + term.tpe.show
//        val r = super.transformTerm(term)(owner)
//        println("!! " + s)
//        println("   " + r.show + " " + r.tpe.show)
//        r

      case PlacedValue(_, placementInfo) if !checkOnly =>
        val transformedTerm = super.transformTerm(term)(owner)
        if placementInfo.modality.subjective then
          transformedTerm match
            case Block(statements, expr) => Block(statements :+ expr, Literal(UnitConstant()))
            case _ => Block(List(transformedTerm), Literal(UnitConstant()))
        else
          transformedTerm

      case _ =>
        if try term.symbol.owner == symbols.placed catch case scala.util.control.NonFatal(_) => false then
          errorAndCancel("Illegal multitier construct.", term.pos)
        super.transformTerm(term)(owner)

    override def transformTypeTree(tree: TypeTree)(owner: Symbol) = tree match
      case Inferred() =>
        if !canceled then
          val eraser = TypePlacementTypesEraser(tree.pos, checkOnly)
          val tpe = eraser.transform(tree.tpe)
          if checkOnly || canceled || tpe == tree.tpe then tree else TypeTree.of(using tpe.asType)
        else
          tree

      case _ =>
        def pos = tree match
          case Applied(tpt, _) => tpt.pos
          case _ => tree.pos

        checkPlacementTypes(tree.tpe, pos)
        val transformTree = super.transformTypeTree(tree)(owner)
        if !canceled then
          TypePlacementTypesEraser(transformTree.pos, checkOnly = true).transform(transformTree.tpe)

        transformTree

    override def transformStatement(stat: Statement)(owner: Symbol) = stat match
      case stat: Definition => transformDefinition(stat)(owner)
      case _ => super.transformStatement(stat)(owner)

    def transformDefinition(stat: Definition)(owner: Symbol) = stat match
      case stat: ClassDef => super.transformStatement(stat)(owner)
      case stat: TypeDef => super.transformStatement(stat)(owner)
      case stat: ValDef => transformValDef(stat)(owner)
      case stat: DefDef => transformDefDef(stat)(owner)

    def transformValDef(stat: ValDef)(owner: Symbol) =
      val symbol = stat.symbol
      val tpt = transformTypeTree(stat.tpt)(symbol)
      val rhs = stat.rhs map { transformTerm(_)(symbol) }
      if checkOnly || canceled || tpt == stat.tpt then
        ValDef.copy(stat)(stat.name, tpt, rhs)
      else
        val info = TypePlacementTypesEraser(stat.pos, checkOnly).transform(stat.symbol.info)
        val privateWithin = if stat.symbol.flags is Flags.Protected then stat.symbol.protectedWithin else stat.symbol.privateWithin
        val symbol = Symbol.newVal(owner, stat.name, info, stat.symbol.flags, privateWithin.fold(Symbol.noSymbol) { _.typeSymbol })
        copyAnnotations(stat.symbol, symbol)
        // TODO: apply references to change to the complete expression
        ValDef(symbol, rhs map { rhs => changeRefs(stat.symbol, symbol, symbol, rhs.changeOwner(symbol)) })

    def transformDefDef(stat: DefDef)(owner: Symbol) =
      val paramss = stat.paramss mapConserve {
        case TypeParamClause(params) => TypeParamClause(transformSubTrees(params)(owner))
        case TermParamClause(params) => TermParamClause(transformSubTrees(params)(owner))
      }
      val tpt = transformTypeTree(stat.returnTpt)(stat.symbol)
      val rhs = stat.rhs map { transformTerm(_)(stat.symbol) }
      if checkOnly || canceled || (paramss == stat.paramss && tpt == stat.returnTpt) then
        DefDef.copy(stat)(stat.name, paramss, tpt, rhs)
      else
        val info = TypePlacementTypesEraser(stat.pos, checkOnly).transform(stat.symbol.info)
        val privateWithin = if stat.symbol.flags is Flags.Protected then stat.symbol.protectedWithin else stat.symbol.privateWithin
        val symbol = Symbol.newMethod(owner, stat.name, info, stat.symbol.flags, privateWithin.fold(Symbol.noSymbol) { _.typeSymbol })
        copyAnnotations(stat.symbol, symbol)
        DefDef(symbol, paramss => rhs map { rhs =>
          val params = (stat.paramss flatMap { _.params map { _.symbol } }) zip (paramss.flatten map { _.symbol })
          (stat.symbol -> symbol :: params).foldLeft(rhs.changeOwner(symbol)) { case (rhs, (statParam, param)) =>
            // TODO: apply references to change to the complete expression
            changeRefs(statParam, param, symbol, rhs)
          }
        })
  end ExpressionPlacementTypesEraser

  private val eraser = ExpressionPlacementTypesEraser(checkOnly = false)
  private val eraserCheckOnly = ExpressionPlacementTypesEraser(checkOnly = true)

  private def erasePlacementTypesFromBody(term: Term, owner: Symbol) =
    transformNormalizedExpression(term, owner, eraserCheckOnly.transformValDef(_)(_), (term, _, symbol) => eraser.transformTerm(term)(symbol))

  private def checkPlacementTypesInArguments(paramss: List[ParamClause], owner: Symbol) =
    paramss foreach { _.params foreach { eraserCheckOnly.transformStatement(_)(owner) } }

  private def checkPlacementTypesInResult(tpt: TypeTree, owner: Symbol) = tpt match
    case Inferred() =>
      val eraser = TypePlacementTypesEraser(tpt.pos, checkOnly = true)
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
    val body = module.body map {
      case stat @ ValDef(name, tpt, Some(rhs)) if !canceled =>
        checkPlacementTypesInResult(tpt, stat.symbol)
        val rhsErased = erasePlacementTypesFromBody(rhs, stat.symbol)
        ValDef.copy(stat)(name, tpt, Some(if canceled then rhs else rhsErased))
      case stat @ DefDef(name, paramss, tpt, Some(rhs)) if !canceled =>
        checkPlacementTypesInArguments(paramss, stat.symbol)
        checkPlacementTypesInResult(tpt, stat.symbol)
        val rhsErased = erasePlacementTypesFromBody(rhs, stat.symbol)
        DefDef.copy(stat)(name, paramss, tpt, Some(if canceled then rhs else rhsErased))
      case stat: Term =>
        val statErased = erasePlacementTypesFromBody(stat, module.symbol)
        if canceled then stat else statErased
      case stat =>
        stat
    }

    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body)
  end erasePlacementTypesFromExpressions
end PlacedExpressions
