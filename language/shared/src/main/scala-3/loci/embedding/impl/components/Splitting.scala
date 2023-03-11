package loci
package embedding
package impl
package components

trait Splitting:
  this: Component with Commons /*with ErrorReporter*/ with Annotations with PlacementInfo with PeerInfo with Synthesis with PlacementContextTypes =>
  import quotes.reflect.*

  def split(module: ClassDef): ClassDef =

//    val fooSym = symbol.declaredMethod("foo").head
//    val fooDef = DefDef(fooSym, argss => Some('{ println(s"Calling foo") }.asTerm))

    val symbol = placedValuesSymbol(module.symbol)
    SymbolMutator.getOrErrorAndAbort.enter(module.symbol, symbol)

    val parents = symbol.typeRef.baseClasses collect:
      case parent if parent != symbol && isMultitierModule(parent.owner) || parent.typeRef =:= types.placedValues =>
        TypeIdent(parent)

    import utility.reflectionExtensions.*

    val symbol2 = peerCommonSymbol(module.symbol)
    val decls2 = symbol2.declarations collect {
      case decl if decl.isField => ValDef(decl, None)
      case decl if decl.isMethod => DefDef(decl, _ => None)
    }

    val tree = ClassDef(
      symbol,
      parents,
      ClassDef(symbol2, List.empty, decls2) :: (PeerInfo.ofModule(module.symbol) map { peerInfo =>
        ClassDef(peerSymbol(peerInfo.peerType.typeSymbol), List.empty, List.empty)
      }))

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

