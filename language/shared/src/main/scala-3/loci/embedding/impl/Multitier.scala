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

  val onLanguage = Symbol.requiredPackage("loci.language.package$package").typeMember("on")
  val onEmbedding = Symbol.requiredPackage("loci.embedding.package$package").typeMember("on")

  @threadUnsafe lazy val symbolMutator = SymbolMutator.make getOrElse
    report.errorAndAbort("Placement type inference not supported with current compiler version. Type needs to be ascribed explicitly.")

  def contextResultCount(tpe: TypeRepr): Int =
    val resultType = tpe.widenTermRefByName.resultType
    if resultType.isContextFunctionType then contextResultCount(resultType.typeArgs.last) + 1 else 0

  object myTreeMap extends TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol) = tree match
      case Ident(name) if name.startsWith("evidence") =>
        tree.tpe.widenTermRefByName.typeArgs.head.asType match
          case '[ t ] =>
            '{ Placement.Context.fallback[t] }.asTerm
      case _ =>
        super.transformTerm(tree)(owner)

  def normalBody(info: TypeRepr, rhs: Option[Term]) =
    info.asType match
      case '[t] => rhs map { rhs => '{ ${rhs.asExpr}; erased: t }.asTerm }

  def stripPlacedExpressionSyntax(info: TypeRepr, rhs: Option[Term]) = normalBody(info, rhs map {
    case Apply(Apply(TypeApply(Select(_, _), _), List(Lambda(_, rhs))), _) => rhs
    case rhs => rhs
  })

  def stripConversion(info: TypeRepr, rhs: Option[Term]) = normalBody(info, rhs map {
    case Lambda(_, Inlined(_, List(conversion: ValDef), Block(List(DefDef("body", List(), _, Some(rhs))), dummy: Typed))) => rhs.underlyingArgument
    case rhs => rhs
  })

  val body = module.body map {
    case stat @ ValDef(name, tpt, rhs) =>
      stat.symbol.info match
        case info @ AppliedType(tycon, _) if tycon.typeSymbol == onLanguage =>
          myTreeMap.transformStatement(ValDef.copy(stat)(name, tpt, stripConversion(info, rhs)))(stat.symbol.owner)
        case AppliedType(tycon, args) if tycon.typeSymbol == onEmbedding =>
          val info = AppliedType(onLanguage.typeRef, args)
          symbolMutator.setInfo(stat.symbol, info)
          // TODO for DefDef
          // val count = contextResultCount(info)
          // if count != 0 then
          //   symbolMutator.updateAnnotation(stat.symbol, TypeRepr.of[scala.annotation.internal.ContextResultCount], List(Literal(IntConstant(count))))
          myTreeMap.transformStatement(ValDef.copy(stat)(name, TypeTree.of(using info.asType), stripPlacedExpressionSyntax(info, rhs)))(stat.symbol.owner)
        case _ =>
          stat
    case stat =>
      stat
  }

  ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body)
end normalizePlacementContextTypes
