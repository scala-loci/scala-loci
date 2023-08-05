package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental

@experimental
trait PlacedTransformations:
  this: Component & Commons & ErrorReporter =>
  import quotes.reflect.*

  def transformBody(term: Term, owner: Symbol)(transform: (Term, List[Symbol]) => Term): Term =
    def transformBody(term: Term, owners: List[Symbol]): Term = term match
      case block @ Lambda(arg :: _, expr) if arg.symbol.isImplicit =>
        val Block(List(lambda: DefDef), closure) = block: @unchecked
        val rhs = transformBody(expr, lambda.symbol :: owners)
        Block.copy(block)(List(DefDef.copy(lambda)(lambda.name, lambda.paramss, lambda.returnTpt, Some(rhs))), closure)
      case _ =>
        transform(term, owners).changeOwner(owners.head)

    transformBody(term, List(owner))
  end transformBody

  private def processPlacedBody(term: Term, transform: Option[(Symbol, Term, Term) => (Term, Option[Term])]) =
    def dropLastExpr(block: Block) = block.statements match
      case (term: Term) +: Nil => term
      case statements :+ (term: Term) => Block.copy(block)(statements, term)
      case statements => Block.copy(block)(statements, Literal(UnitConstant()))

    def appendExpr(original: Block)(term: Term, expr: Term) = term match
      case Lambda(_, _) => Block.copy(original)(List(term), expr)
      case block @ Block(statements, Literal(UnitConstant())) => Block.copy(block)(statements, expr)
      case block @ Block(statements, _) => Block.copy(block)(statements :+ block.expr, expr)
      case _ => Block.copy(original)(List(term), expr)

    term match
      case Block(List(lambda @ DefDef(name, args @ List(TermParamClause(List(arg))), tpt, Some(block @ Block(_, erased: Typed)))), closure: Closure)
          if arg.symbol.isImplicit &&
             !(arg.symbol.info =:= TypeRepr.of[Nothing]) && arg.symbol.info <:< types.context &&
             erased.tpe.typeSymbol == symbols.`embedding.on` =>
        val body = dropLastExpr(block)
        transform.fold(body): transform =>
          val (rhs, expr) = transform(lambda.symbol, body, erased)
          Block.copy(term)(List(DefDef.copy(lambda)(name, args, tpt, Some(expr.fold(rhs) { appendExpr(block)(rhs, _) }))), closure)
      case _ =>
        errorAndCancel("Unexpected shape of placed expression.", term.posInUserCode)
        term
  end processPlacedBody

  def transformPlacedBody(term: Term)(transform: (Symbol, Term, Term) => (Term, Option[Term])): Term =
    processPlacedBody(term, Some(transform))

  def extractPlacedBody(term: Term): Term =
    processPlacedBody(term, None)
end PlacedTransformations
