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
       !(tpe =:= TypeRepr.of[Nothing]) && (tpe <:< types.context || tpe <:< types.placedValue || tpe <:< types.subjective) then
      errorAndCancel("Illegal multitier construct.", pos)

  private class TypePlacementTypesEraser(pos: Position, checkOnly: Boolean) extends SimpleTypeMap(quotes):
    override def transform(tpe: TypeRepr) =
      val erasedType =
        if !checkOnly then
          PlacementInfo(tpe).fold(super.transform(tpe)) { placementInfo =>
            if placementInfo.subjective.isDefined then
              TypeRepr.of[Unit]
            else
              transform(placementInfo.valueType)
          }
        else
          super.transform(tpe)

      checkPlacementTypes(erasedType, pos)
      erasedType
  end TypePlacementTypesEraser

  private class ExpressionPlacementTypesEraser(checkOnly: Boolean) extends TreeMap:
    override def transformTerm(term: Term)(owner: Symbol) = term match
      case Inlined(Some(call), (conversion: ValDef) :: _, Block(List(DefDef(names.body, List(), _, _)), erased: Typed))
        if call.symbol == symbols.placed.companionModule.moduleClass &&
           !(conversion.tpt.tpe =:= TypeRepr.of[Nothing]) && conversion.tpt.tpe <:< types.conversion &&
           !(erased.tpe =:= TypeRepr.of[Nothing]) && erased.tpe <:< types.placed =>
        if !canceled then
          errorAndCancel("Illegal subjective access", term.pos)
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
        if placementInfo.subjective.isDefined then
          '{ ${ transformedTerm.asExpr }; () }.asTerm.underlyingArgument
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
          val tpe = eraser.transform(tree.tpe).asType
          if checkOnly then tree else TypeTree.of(using tpe)
        else
          tree

      case _ =>
        def pos = tree match
          case Applied(tpt, _) => tpt.pos
          case _ => tree.pos

        checkPlacementTypes(tree.tpe, pos)
        super.transformTypeTree(tree)(owner)

    def transformValDef(stat: ValDef)(owner: Symbol) =
      val symbol = stat.symbol
      val tpt = transformTypeTree(stat.tpt)(symbol)
      val rhs = stat.rhs map { transformTerm(_)(symbol) }
      ValDef.copy(stat)(stat.name, tpt, rhs)
  end ExpressionPlacementTypesEraser

  private val eraser = ExpressionPlacementTypesEraser(checkOnly = false)
  private val eraserCheckOnly = ExpressionPlacementTypesEraser(checkOnly = true)

  private def erasePlacementTypesFromBody(term: Term, owner: Symbol) =
    transformNormalizedExpression(term, owner, eraserCheckOnly.transformValDef(_)(_), eraser.transformTerm(_)(_))

  private def checkPlacementTypesInArguments(paramss: List[ParamClause], owner: Symbol) =
    paramss foreach { _.params foreach { eraserCheckOnly.transformStatement(_)(owner) } }

  private def checkPlacementTypesInResult(tpt: TypeTree, owner: Symbol) = tpt match
    case Inferred() =>
      val eraser = TypePlacementTypesEraser(tpt.pos, checkOnly = true)
      PlacementInfo(tpt.tpe).fold(eraser.transform(tpt.tpe)) { placementInfo =>
        eraser.transform(placementInfo.valueType)
        eraser.transform(placementInfo.peerType)
        placementInfo.subjective foreach eraser.transform
      }
    case Applied(on: TypeIdent, List(value, peer)) if on.symbol == symbols.`language.on` => value match
      case Applied(per: TypeIdent, List(value, subjective)) if per.symbol == symbols.`language.per` =>
        eraserCheckOnly.transformTree(subjective)(owner)
        eraserCheckOnly.transformTree(value)(owner)
        eraserCheckOnly.transformTree(peer)(owner)
      case _ =>
        eraserCheckOnly.transformTree(value)(owner)
        eraserCheckOnly.transformTree(peer)(owner)
    case _ =>
      eraserCheckOnly.transformTree(tpt)(owner)

  // TODO: extend SimpleTypeMap

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
