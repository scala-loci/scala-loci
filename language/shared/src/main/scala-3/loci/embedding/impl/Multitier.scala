package loci
package embedding
package impl

import embedding.*
import utility.reflectionExtensions.*

import scala.annotation.threadUnsafe
import scala.quoted.*

object Multitier:
  def annotation(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    tree match
      case tree: ClassDef =>
        List(normalizePlacementContextTypes(tree))
      case _ =>
        report.errorAndAbort("multitier annotation only applicable to classes, traits or objects")

def normalizePlacementContextTypes(using Quotes)(module: quotes.reflect.ClassDef): quotes.reflect.ClassDef =
  import quotes.reflect.*

  object symbols:
    val `language.on` = Symbol.requiredPackage("loci.language.package$package").typeMember("on")
    val `embedding.on` = Symbol.requiredPackage("loci.embedding.package$package").typeMember("on")
    val on = TypeRepr.of[Placement.On[_]].typeSymbol
    val placed = TypeRepr.of[Placed.type].typeSymbol
    val contextResultCount = TypeRepr.of[annotation.internal.ContextResultCount].typeSymbol

  object types:
    val placed = TypeRepr.of[Placed[_, _]]
    val conversion = TypeRepr.of[Conversion[_, _]]

  val optionalSymbolMutator = SymbolMutator.make

  @threadUnsafe lazy val symbolMutator = optionalSymbolMutator getOrElse
    report.errorAndAbort("Placement type inference not supported with current compiler version. Type needs to be ascribed explicitly.")

  def replaceResultType(tpe: TypeRepr, res: TypeRepr): TypeRepr = tpe match
    case MethodType(paramNames, paramTypes, resType) =>
      MethodType(paramNames)(_ => paramTypes, _ => replaceResultType(resType, res))
    case PolyType(paramNames, paramBounds, resType) =>
      MethodType(paramNames)(_ => paramBounds, _ => replaceResultType(resType, res))
    case ByNameType(underlying) =>
      ByNameType(replaceResultType(underlying, res))
    case _ =>
      res

//  def contextResultCount(tpe: TypeRepr): Int =
//    if tpe.isContextFunctionType then contextResultCount(tpe.typeArgs.last) + 1 else 0

  def contextResultCount(symbol: Symbol) =
    symbol.getAnnotation(symbols.contextResultCount) match
      case Some(Apply(_, List(Literal(IntConstant(count))))) => count
      case _ => 0

  def tryIncreasingContextResultCount(symbol: Symbol) =
    optionalSymbolMutator foreach { symbolMutator =>
      symbolMutator.updateAnnotation(
        symbol,
        symbols.contextResultCount,
        List(Literal(IntConstant(contextResultCount(symbol) + 1))))
    }

  def normalBody(symbol: Symbol, arg: TypeRepr, res: TypeRepr, rhs: Option[Term]) =
    rhs map { rhs =>
      (arg.asType, res.asType) match
        case ('[a], '[r]) =>
          given Quotes = symbol.asQuotes
          '{ (_: Placement.Context[a]) ?=> ${rhs.asExpr}; erased: embedding.on[r, a] }.asTerm
    }

  def stripPlacedExpressionSyntax(symbol: Symbol, arg: TypeRepr, res: TypeRepr, rhs: Option[Term]) =
    normalBody(symbol, arg, res, rhs map {
      case Apply(Apply(fun, List(Lambda(_, rhs))), _) if fun.symbol.owner == symbols.on => rhs
      case rhs => rhs
    })

  def stripConversion(symbol: Symbol, arg: TypeRepr, res: TypeRepr, rhs: Option[Term]) =
    normalBody(symbol, arg, res, rhs map {
      case x @ Lambda(_, Inlined(Some(call), List(conversion: ValDef), Block(List(DefDef("body", List(), _, Some(rhs))), dummy: Typed)))
        if call.symbol == symbols.placed &&
           conversion.tpt.tpe <:< types.conversion &&
           dummy.tpe <:< types.placed =>
        println("!! " + x.show)
        rhs.underlyingArgument
      case x @ Lambda(_, Inlined(Some(call), List(conversion: ValDef, ValDef(_, _, Some(rhs))), Block(List(DefDef("body", List(), _, _)), dummy: Typed)))
        if call.symbol == symbols.placed &&
           conversion.tpt.tpe <:< types.conversion &&
           dummy.tpe <:< types.placed =>
        println("!! " + x.show)
        rhs.underlyingArgument
      case rhs =>
        println("!! " + rhs.show)
        println("   " + rhs)
        rhs
    })

  val body = module.body map {
    case stat @ ValDef(name, tpt, rhs) =>
      val symbol = stat.symbol
      symbol.info match
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`language.on` =>
          ValDef.copy(stat)(name, tpt, stripConversion(symbol, args.last, args.head, rhs))
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`embedding.on` =>
          val info = AppliedType(symbols.`language.on`.typeRef, args)
          symbolMutator.setInfo(symbol, info)
          ValDef.copy(stat)(name, TypeTree.of(using info.asType), stripPlacedExpressionSyntax(symbol, args.last, args.head, rhs))
        case _ =>
          // TODO: check error
          stat
    case stat @ DefDef(name, paramss, tpt, rhs) =>
      val symbol = stat.symbol
      println("###################### " + symbol.info.show)
      symbol.info.resultType match
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`language.on` =>
          DefDef.copy(stat)(name, paramss, tpt, stripConversion(symbol, args.last, args.head, rhs))
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`embedding.on` =>
          val info = AppliedType(symbols.`language.on`.typeRef, args)
          tryIncreasingContextResultCount(symbol)
          symbolMutator.setInfo(symbol, replaceResultType(symbol.info, info))
          DefDef.copy(stat)(name, paramss, TypeTree.of(using info.asType), stripPlacedExpressionSyntax(symbol, args.last, args.head, rhs))
        case _ =>
          // TODO: check error
          stat
    case stat =>
      stat
  }

  object myTreeMap extends TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol) = tree match
      case Ident(name) if name.startsWith("evidence") =>
        tree.tpe.widenTermRefByName.typeArgs.head.asType match
          case '[ t ] =>
            '{ Placement.Context.fallback[t] }.asTerm
      case _ =>
        super.transformTerm(tree)(owner)

  ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body map { stat =>
    myTreeMap.transformStatement(stat)(stat.symbol.owner)
  })
end normalizePlacementContextTypes
