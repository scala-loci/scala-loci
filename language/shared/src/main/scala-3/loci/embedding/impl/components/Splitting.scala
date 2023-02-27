package loci
package embedding
package impl
package components

//import utility.reflectionExtensions.*

import scala.quoted.*

trait Splitting:
  this: Component with Commons /*with ErrorReporter*/ with PlacementInfo with PlacementContextTypes =>
  import quotes.reflect.*

  def split(module: ClassDef): ClassDef =
    val parents = List(TypeTree.of[Object], TypeTree.of[loci.runtime.PlacedValues])
    def decls(cls: Symbol) = List.empty[Symbol]
    val symbol = Symbol.newClass(module.symbol, names.module(module.symbol), parents map { _.tpe }, decls, selfType = None)
//    val symbol = Symbol.newClass(module.symbol, s"<placed values of ${fullName(module.symbol)}>", parents map { _.tpe }, decls, selfType = None)
    SymbolMutator.getOrErrorAndAbort.setFlag(symbol, Flags.Trait | Flags.Synthetic)

//    val fooSym = symbol.declaredMethod("foo").head
//    val fooDef = DefDef(fooSym, argss => Some('{ println(s"Calling foo") }.asTerm))

    val tree = ClassDef(symbol, parents.tail, List.empty)

//    SymbolMutator.getOrErrorAndAbort.enter(module.symbol, symbol)
//    block.statements foreach { stat => SymbolMutator.getOrErrorAndAbort.enter(module.symbol, stat.symbol) }

    def eraseBody(term: Term, owner: Symbol) =
      transformNormalizedExpression(term, owner,
        (expr, _) => expr,
        (_, _, expr) => Literal(NullConstant()).select(symbols.asInstanceOf).appliedToType(expr.tpe) -> None)

    val body = module.body collect:
      case PlacedStatement(stat @ ValDef(name, tpt, rhs)) =>
        ValDef.copy(stat)(name, tpt, rhs map { eraseBody(_, stat.symbol) })
      case PlacedStatement(stat @ DefDef(name, paramss, tpt, rhs)) =>
        DefDef.copy(stat)(name, paramss, tpt, rhs map { eraseBody(_, stat.symbol) })
      case stat: Definition =>
        stat
      // TODO: also erase non-placed statements
      // TODO: in erasure include *all* context closures

    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body :+ tree)
  end split
end Splitting

