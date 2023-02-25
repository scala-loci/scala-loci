package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.reflect.TypeTest
import scala.quoted.*

trait Commons:
  this: Component =>
  import quotes.reflect.*

  object symbols:
    val `language.Local` = Symbol.requiredPackage("loci.language.package$package").typeMember("Local")
    val `language.per` = Symbol.requiredPackage("loci.language.package$package").typeMember("per")
    val `language.on` = Symbol.requiredPackage("loci.language.package$package").typeMember("on")
    val `embedding.on` = Symbol.requiredPackage("loci.embedding.package$package").typeMember("on")
    val `Placed.on` = Symbol.requiredPackage("loci.embedding.Placed").typeMember("on")
    val `Placed.Subjective.on` = Symbol.requiredPackage("loci.embedding.Placed.Subjective").typeMember("on")
    val on = TypeRepr.of[Placement.On[?]].typeSymbol
    val placed = TypeRepr.of[Placed[?, ?]].typeSymbol
    val subjective = TypeRepr.of[Placed.Subjective[?, ?]].typeSymbol
    val multitier = TypeRepr.of[language.multitier].typeSymbol
    val function1 = TypeRepr.of[Function1[?, ?]].typeSymbol
    val contextFunction1 = TypeRepr.of[ContextFunction1[?, ?]].typeSymbol
    val contextResultCount = TypeRepr.of[annotation.internal.ContextResultCount].typeSymbol
    val erased = '{ embedding.erased }.asTerm.underlyingArgument.symbol

  object types:
    val placedValue = TypeRepr.of[PlacedValue[?, ?]]
    val placed = TypeRepr.of[Placed[?, ?]]
    val subjective = TypeRepr.of[Placed.Subjective[?, ?]]
    val remote = TypeRepr.of[language.Remote[?]]
    val context = TypeRepr.of[Placement.Context[?]]
    val conversion = TypeRepr.of[Conversion[?, ?]]

  object names:
    val sbj = "sbj"
    val body = "body"
    val apply = "apply"

  final class PackedValueType[T](using t: Type[T]):
    opaque type Type = T
    given quoted.Type[Type] = t

//  final class PackedValueType[T](using t: Type[T]):
//    opaque type Type1 = T
//    opaque type Type2 = T
//    opaque type Type3 = T
//    opaque type Type4 = T
//    given Type[Type1] = t
//    given Type[Type2] = t
//    given Type[Type3] = t
//    given Type[Type4] = t

  extension (tpe: TypeRepr) def asPackedValueType: PackedValueType[?] = tpe.asType match
    case t: Type[Any] @unchecked if tpe <:< TypeRepr.of[Any] => PackedValueType(using t)
    case _ => throw IllegalArgumentException(s"${tpe.safeShow} cannot be used as a value type")

  given ValOrDefDef: TypeTest[Tree, ValDef | DefDef] = tree =>
    summon[TypeTest[Tree, ValDef]].unapply(tree) orElse
    summon[TypeTest[Tree, DefDef]].unapply(tree)

  def isMultitierModule(symbol: Symbol): Boolean =
    symbol.getAnnotation(symbols.multitier).isDefined

  def fullName(symbol: Symbol): String =
    def fullName(symbol: Symbol, name: String): String =
      val owner = symbol.owner

      val symbolName = symbol.name
//        if (symbol.name startsWith "$loci$multitier$")
//          (owner.info member TermName(symbol.name.drop(16)) orElse symbol).name.toString
//        else
//          symbol.name

      if owner.exists && ((symbol.flags is Flags.Synthetic) || (symbolName startsWith "<") && (symbolName endsWith ">")) then
        fullName(owner, name)
      else
        val prefix = if owner.isNoSymbol || owner == defn.RootClass then symbolName else fullName(owner, symbolName)

        if prefix.isEmpty then
          name
        else if name.isEmpty then
          prefix
        else
          val separator = if symbol.isType && !symbol.isPackageDef && !symbol.isModuleDef then "#" else "."
          val suffix = if symbol.isType && symbol.isModuleDef && (name endsWith "$") then name.dropRight(1) else name
          s"$prefix$separator$suffix"
    end fullName

    fullName(symbol, "")
  end fullName
end Commons
