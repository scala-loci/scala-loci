package loci
package embedding
package impl
package components

//import utility.reflectionExtensions.*

import scala.quoted.*

trait Splitting:
//  this: Component with Commons with Annotations with PlacementInfo =>
  this: Component with Commons with PlacementInfo =>
  import quotes.reflect.*

  def split(module: ClassDef): ClassDef =
    val parents = List(TypeTree.of[Object], TypeTree.of[loci.runtime.PlacedValues])
    def decls(cls: Symbol) = List.empty[Symbol]
    val symbol = Symbol.newClass(module.symbol, s"<placed values of ${fullName(module.symbol)}>", parents map { _.tpe }, decls, selfType = None)
    SymbolMutator.getOrErrorAndAbort.setFlag(symbol, Flags.Trait)

//    val fooSym = symbol.declaredMethod("foo").head
//    val fooDef = DefDef(fooSym, argss => Some('{ println(s"Calling foo") }.asTerm))

    val tree = ClassDef(symbol, parents.tail, List.empty)

    SymbolMutator.getOrErrorAndAbort.enter(module.symbol, symbol)
    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, module.body :+ tree)
  end split
end Splitting

