package loci
package language

import embedding._
import utility.reflectionExtensions.*

import scala.annotation.MacroAnnotation
import scala.quoted._

//@experimental
class multitier extends MacroAnnotation:
  //def this(accessorGeneration: AccessorGeneration) = this()

  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    tree match
//      case ClassDef(name, ctr, parents, self, List(d @ ValDef(defname, tpt, rhs))) =>
      case ClassDef(name, ctr, parents, self, body) =>
//        val dd = ValDef.copy(d)(defname, TypeTree.of[Int ?=> String], Some('{ (x: Int) ?=> "test" }.asTerm))

        val onLanguage = Symbol.requiredPackage("loci.language.package$package").typeMember("on")
        val onEmbedding = Symbol.requiredPackage("loci.embedding.package$package").typeMember("on")

        def contextResultCount(tpe: TypeRepr): Int =
          val resultType = tpe.widenTermRefByName.resultType
          if resultType.isContextFunctionType then contextResultCount(resultType.typeArgs.last) + 1 else 0

        // '{ annotation.internal.ContextResultCount(${Expr(contextResultCount(stat.symbol.info))}) }.asTerm.underlyingArgument

        object myTreeMap extends TreeMap:
          override def transformTerm(tree: Term)(owner: Symbol) = tree match
            case Ident(name) if name.startsWith("evidence") =>
              tree.tpe.widenTermRefByName.typeArgs.head.asType match
                case '[ t ] =>
                  '{ Placement.Context.fallback[t] }.asTerm
            case _ =>
              super.transformTerm(tree)(owner)

        val cleaned = body map {
          case stat @ ValDef(name, tpt, rhs) =>
            stat.symbol.info match
              case info @ AppliedType(tycon, _) if tycon.typeSymbol == onLanguage =>
                info.asType match
                  case '[t] =>
                    rhs match
                        case Some(Lambda(_, Inlined(_, List(conversion: ValDef), Block(List(DefDef("body", List(), _, Some(rhs))), dummy: Typed)))) =>
                          myTreeMap.transformStatement(ValDef.copy(stat)(name, tpt, Some(Block(List(rhs.underlyingArgument), '{ erased: t }.asTerm))))(stat.symbol.owner)
                        case _ =>
                          ???
              case AppliedType(tycon, args) if tycon.typeSymbol == onEmbedding =>
                val info = AppliedType(onLanguage.typeRef, args)
                info.asType match
                  case '[ t ] =>
                    rhs match
                      case Some(Apply(Apply(TypeApply(Select(_, _), _), List(Lambda(_, rhs))), _)) =>
                        val cleaned = myTreeMap.transformStatement(ValDef.copy(stat)(name, TypeTree.of[t], Some(Block(List(rhs), '{ erased: t }.asTerm))))(stat.symbol.owner)

                        val quotesImplClass = Class.forName("scala.quoted.runtime.impl.QuotesImpl")
                        val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
                        val typeClass = Class.forName("dotty.tools.dotc.core.Types$Type")
                        val symbolClass = Class.forName("dotty.tools.dotc.core.Symbols$Symbol")
                        val symDenotationClass = Class.forName("dotty.tools.dotc.core.SymDenotations$SymDenotation")

                        val ctx = quotesImplClass.getMethod("ctx")
                        val denot = symbolClass.getMethod("denot", contextClass)
                        val infoXXX = symDenotationClass.getMethod("info_$eq", typeClass)
                        // val annotations = symDenotationClass.getMethod("annotations_$eq", Class.forName("scala.collection.immutable.List"))

                        val context = ctx.invoke(quotes)
                        infoXXX.invoke(denot.invoke(stat.symbol, context), info)
                        // annotations.invoke(denot.invoke(stat.symbol, context), List.empty)

                        cleaned
                      case _ =>
                        ???
              case _ =>
                stat
          case stat =>
            stat
        }

//        val x = '{
//          def a: Int ?=> String = ???
//        }
//
//        x.asTerm.underlyingArgument.asInstanceOf[Block].statements foreach { stat =>
//          println(stat.show)
//          println(stat.symbol.annotations)
//          println('{ annotation.internal.ContextResultCount(${Expr(contextResultCount(stat.symbol.info))}) }.asTerm.underlyingArgument)
//        }

//        cleaned foreach { stat => println(stat.show) }

        List(ClassDef.copy(tree)(name, ctr, parents, self, cleaned))
//        List(tree)

      case _ =>
        report.errorAndAbort("Annotation only supports `class`")

//object multitier {
//  def start[P, Inst[P] <: Instance[P]](instance: Inst[P]): Runtime[P] =
//  macro impl.Instance.start
//
//  @compileTimeOnly("method can only be invoked in multitier code")
//  def running: Boolean = erased
//
//  @compileTimeOnly("method can only be invoked in multitier code")
//  def terminate(): Unit = erased
//}
