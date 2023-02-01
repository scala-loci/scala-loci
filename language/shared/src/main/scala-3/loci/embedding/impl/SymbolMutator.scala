package loci
package embedding
package impl

import scala.quoted.*

class SymbolMutator private ():
  private val quotesImplClass = Class.forName("scala.quoted.runtime.impl.QuotesImpl")
  private val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
  private val typeClass = Class.forName("dotty.tools.dotc.core.Types$Type")
  private val symbolClass = Class.forName("dotty.tools.dotc.core.Symbols$Symbol")
  private val symDenotationClass = Class.forName("dotty.tools.dotc.core.SymDenotations$SymDenotation")
  private val classDenotationClass = Class.forName("dotty.tools.dotc.core.SymDenotations$ClassDenotation")
  private val scopeClass = Class.forName("dotty.tools.dotc.core.Scopes$Scope")
  private val emptyScopeClass = Class.forName("dotty.tools.dotc.core.Scopes$EmptyScope$")
  private val annotationClass = Class.forName("dotty.tools.dotc.core.Annotations$Annotation")

  private val ctx = quotesImplClass.getMethod("ctx")
  private val denot = symbolClass.getMethod("denot", contextClass)
  private val span = symbolClass.getMethod("span")
  private val infoSet = symDenotationClass.getMethod("info_$eq", typeClass)
  private val flagSet = symDenotationClass.getMethod("setFlag", classOf[Long])
  private val flagReset = symDenotationClass.getMethod("resetFlag", classOf[Long])
//  private val annotationRemove = symDenotationClass.getMethod("removeAnnotation", symbolClass, contextClass)
  private val annotationUpdate = symDenotationClass.getMethod("updateAnnotation", annotationClass, contextClass)
  private val enterSymbol = classDenotationClass.getMethod("enter", symbolClass, scopeClass, contextClass)
  private val emptyScope = emptyScopeClass.getField("MODULE$")
  private val annotationApply = annotationClass.getMethod("apply", typeClass, classOf[List[_]], classOf[Long], contextClass)

  def setInfo(using Quotes)(symbol: quotes.reflect.Symbol, info: quotes.reflect.TypeRepr): Unit =
    infoSet.invoke(denot.invoke(symbol, ctx.invoke(quotes)), info)

  def enter(using Quotes)(owner: quotes.reflect.Symbol, symbol: quotes.reflect.Symbol): Unit =
    val context = ctx.invoke(quotes)
    val denotation = denot.invoke(owner, context)
    if classDenotationClass.isInstance(denotation) then
      enterSymbol.invoke(denotation, symbol, emptyScope.get(null), context)

  def setFlag(using Quotes)(symbol: quotes.reflect.Symbol, flags: quotes.reflect.Flags) =
    flagSet.invoke(denot.invoke(symbol, ctx.invoke(quotes)), flags)

  def resetFlag(using Quotes)(symbol: quotes.reflect.Symbol, flags: quotes.reflect.Flags) =
    flagReset.invoke(denot.invoke(symbol, ctx.invoke(quotes)), flags)

  def updateAnnotation(using Quotes)(symbol: quotes.reflect.Symbol, annotation: quotes.reflect.Symbol, args: List[quotes.reflect.Term]): Unit =
    val context = ctx.invoke(quotes)
    annotationUpdate.invoke(
      denot.invoke(symbol, context),
      annotationApply.invoke(null, annotation.typeRef, args, span.invoke(symbol), context),
      context)

//  def removeAnnotation(using Quotes)(symbol: quotes.reflect.Symbol, annotation: quotes.reflect.Symbol): Unit =
//    val context = ctx.invoke(quotes)
//    annotationRemove.invoke(denot.invoke(symbol, context), annotation, context)

object SymbolMutator:
  private val instance =
    try Some(SymbolMutator())
    catch case _: ClassNotFoundException | _ : NoSuchMethodException => None

  def get: Option[SymbolMutator] = instance

  def getOrErrorAndAbort(using Quotes): SymbolMutator =
    instance.getOrElse(quotes.reflect.report.errorAndAbort("Multitier modules not supported with current compiler version."))
