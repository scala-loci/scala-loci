package loci
package embedding
package impl

import components.*

import scala.annotation.experimental
import scala.quoted.*
import scala.util.control.NonFatal

@experimental
def inferrableCanonicalPlacementTypeContextClosure[T: Type, R: Type](using Quotes)(v: Expr[Any]*): Expr[R] =
  import quotes.reflect.*

  object info extends Component.withQuotes(quotes), Commons, PlacementInfo

  def clean(tpe: TypeRepr) = tpe.asType match
    case '[ t `on` p ] =>
      val local = TypeRepr.of[t].typeSymbol == info.symbols.`language.Local`
      PlacedClean.cleanType[t on p, p, t] match
        case '[ u ] =>
          val u = if local then info.symbols.`language.Local`.typeRef.appliedTo(TypeRepr.of[u]) else TypeRepr.of[u]
          info.symbols.`embedding.on`.typeRef.appliedTo(List(u, TypeRepr.of[p]))
    case '[ r ] =>
      TypeRepr.of[r]

  def canonical(tpe: TypeRepr) =
    info.PlacementInfo(tpe).fold(tpe): placementInfo =>
      info.symbols.`embedding.on`.typeRef.appliedTo(placementInfo.canonicalType.typeArgs)

  val r = canonical(clean(TypeRepr.of[R]))

  val result = Block(v.toList map { _.asTerm }, Typed(Ref('{ erased }.asTerm.underlyingArgument.symbol), TypeTree.of(using r.asType)))

  // To make the context function type inferrable, we hack the current context and change its mode to `Pattern`
  // as this mode lets the context function type propagate without resolving the context argument:
  // https://github.com/lampepfl/dotty/blob/3.3.0/compiler/src/dotty/tools/dotc/typer/Typer.scala#L3790
  //
  // This hack is without unwanted side effects since we ensure that the expanding function
  // is the outer-most in the surrounding val or def.
  // Hence, no further type-checking will happen in the current context.
  try
    r match
      case AppliedType(fun, typeArgs @ List(_, peer)) if fun.typeSymbol == info.symbols.`embedding.on` =>
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
                    (r.asType, peer.asType) match
                      case ('[ r ], '[ p ]) =>
                        Lambda(symbol, info.contextMethodType[Placement.Context[p], r], (symbol, _) => result.changeOwner(symbol)): @unchecked

                  Block.copy(block)(List(lambda), Closure.copy(closure)(meth, Some(info.symbols.`language.on`.typeRef.appliedTo(typeArgs)))).asExpr match
                    case result: Expr[R] @unchecked => return result

                case _ =>
      case _ =>
  catch
    case NonFatal(e) =>

  result.asExpr match
    case result: Expr[R] @unchecked => result
end inferrableCanonicalPlacementTypeContextClosure
