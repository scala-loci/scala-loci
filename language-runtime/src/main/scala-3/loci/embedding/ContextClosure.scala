package loci
package embedding

import scala.annotation.experimental
import scala.quoted.*
import scala.util.control.NonFatal

@experimental
def inferrablePlacementContextClosure[T: Type](using Quotes)(v: Expr[Any]*): Expr[T] =
  import quotes.reflect.*

  def contextMethodType[T: Type, R: Type] =
    val Inlined(_, _, Block(List(lambda), _)) = '{ (_: T) ?=> erased : R }.asTerm: @unchecked
    val tpe @ MethodType(_, _, _) = lambda.symbol.info: @unchecked
    tpe

  val result = Block(v.toList map { _.asTerm }, Typed(Ref('{ erased }.asTerm.underlyingArgument.symbol), TypeTree.of[T]))

  val `language.on` = Symbol.requiredPackage("loci.language.package$package").typeMember("on")
  val `embedding.on` = Symbol.requiredPackage("loci.embedding.package$package").typeMember("on")

  // To make the context function type inferrable, we hack the current context and change its mode to `Pattern`
  // as this mode lets the context function type propagate without resolving the context argument:
  // https://github.com/lampepfl/dotty/blob/3.3.0/compiler/src/dotty/tools/dotc/typer/Typer.scala#L3790
  //
  // This hack is without unwanted side effects since we ensure that the expanding function
  // is the outer-most in the surrounding val or def.
  // Hence, no further type-checking will happen in the current context.
  try
    TypeRepr.of[T] match
      case AppliedType(fun, typeArgs @ List(_, peer)) if fun.typeSymbol == `embedding.on` =>
        val symbol = Symbol.spliceOwner.owner
        if symbol.isDefDef || symbol.isValDef then
          val quotesImplClass = Class.forName("scala.quoted.runtime.impl.QuotesImpl")
          val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
          val freshContextClass = Class.forName("dotty.tools.dotc.core.Contexts$FreshContext")
          val modeClass = Class.forName("dotty.tools.dotc.core.Mode")
          val symbolClass = Class.forName("dotty.tools.dotc.core.Symbols$Symbol")
          val symDenotationClass = Class.forName("dotty.tools.dotc.core.SymDenotations$SymDenotation")
          val completerClass = Class.forName("dotty.tools.dotc.typer.Namer$Completer")
          val sourcePositionClass = Class.forName("dotty.tools.dotc.util.SourcePosition")
          val positionedClass = Class.forName("dotty.tools.dotc.ast.Positioned")
          val blockClass = Class.forName("dotty.tools.dotc.ast.Trees$Block")
          val valOrDefDefClass = Class.forName("dotty.tools.dotc.ast.Trees$ValOrDefDef")

          val ctx = quotesImplClass.getMethod("ctx")
          val outersIterator = contextClass.getMethod("outersIterator")
          val owner = freshContextClass.getMethod("owner")
          val setMode = freshContextClass.getMethod("setMode", classOf[Int])
          val denot = symbolClass.getMethod("denot", contextClass)
          val infoOrCompleter = symDenotationClass.getMethod("infoOrCompleter")
          val original = completerClass.getMethod("original")
          val contains = sourcePositionClass.getMethod("contains", sourcePositionClass)
          val sourcePos = positionedClass.getMethod("sourcePos", contextClass)
          val stats = blockClass.getMethod("stats")
          val expr = blockClass.getMethod("expr")
          val unforcedRhs = valOrDefDefClass.getMethod("unforcedRhs")

          val context = ctx.invoke(quotes)
          val pattern = modeClass.getMethod("Pattern").invoke(null)
          val completer = infoOrCompleter.invoke(denot.invoke(symbol, context))

          if completerClass.isInstance(completer) then
            val rhs = unforcedRhs.invoke(original.invoke(completer))
            val term =
              if blockClass.isInstance(rhs) then
                stats.invoke(rhs) match
                  case stats: List[?] if stats.isEmpty => expr.invoke(rhs)
                  case _ => rhs
              else rhs

            if contains.invoke(Position.ofMacroExpansion, sourcePos.invoke(term, context)) == true then
              outersIterator.invoke(context) match
                case outers: Iterator[?] =>
                  outers foreach: context =>
                    if freshContextClass.isInstance(context) && (owner.invoke(context) eq symbol) then
                      setMode.invoke(context, pattern)

                  val block @ Block(List(lambda: DefDef), closure @ Closure(meth, _)) =
                    peer.asType match
                      case '[ p ] =>
                        Lambda(symbol, contextMethodType[Placement.Context[p], T], (symbol, _) => result.changeOwner(symbol)): @unchecked

                  Block.copy(block)(List(lambda), Closure.copy(closure)(meth, Some(`language.on`.typeRef.appliedTo(typeArgs)))).asExpr match
                    case block: Expr[T] @unchecked => return block

                case _ =>
      case _ =>
  catch
    case NonFatal(e) =>

  result.asExprOf[T]
end inferrablePlacementContextClosure
