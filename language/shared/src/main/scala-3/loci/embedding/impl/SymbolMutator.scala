package loci
package embedding
package impl

import scala.quoted.*

class SymbolMutator private ():
  private val quotesImplClass = Class.forName("scala.quoted.runtime.impl.QuotesImpl")
  private val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
  private val treeClass = Class.forName("dotty.tools.dotc.ast.Trees$Tree")
  private val defdefClass = Class.forName("dotty.tools.dotc.ast.Trees$DefDef")
  private val typeClass = Class.forName("dotty.tools.dotc.core.Types$Type")
  private val symbolsClass = Class.forName("dotty.tools.dotc.core.Symbols")
  private val symbolClass = Class.forName("dotty.tools.dotc.core.Symbols$Symbol")
  private val symDenotationClass = Class.forName("dotty.tools.dotc.core.SymDenotations$SymDenotation")
  private val classDenotationClass = Class.forName("dotty.tools.dotc.core.SymDenotations$ClassDenotation")
  private val scopeClass = Class.forName("dotty.tools.dotc.core.Scopes$Scope")
  private val emptyScopeClass = Class.forName("dotty.tools.dotc.core.Scopes$EmptyScope$")
  private val annotationClass = Class.forName("dotty.tools.dotc.core.Annotations$Annotation")
  private val contextFunctionResultsClass = Class.forName("dotty.tools.dotc.transform.ContextFunctionResults$")

  private val ctx = quotesImplClass.getMethod("ctx")
  private val newLocalDummy = symbolsClass.getMethod("newLocalDummy", symbolClass, classOf[Int], contextClass)
  private val denot = symbolClass.getMethod("denot", contextClass)
  private val span = symbolClass.getMethod("span")
  private val infoSet = symDenotationClass.getMethod("info_$eq", typeClass)
  private val flagSet = symDenotationClass.getMethod("setFlag", classOf[Long])
  private val flagReset = symDenotationClass.getMethod("resetFlag", classOf[Long])
//  private val companionRegister = symDenotationClass.getMethod("registerCompanion", symbolClass, contextClass)
  private val annotationRemove = symDenotationClass.getMethod("removeAnnotation", symbolClass, contextClass)
  private val annotationUpdate = symDenotationClass.getMethod("updateAnnotation", annotationClass, contextClass)
  private val enterSymbol = classDenotationClass.getMethod("enter", symbolClass, scopeClass, contextClass)
  private val replaceSymbol = classDenotationClass.getMethod("replace", symbolClass, symbolClass, contextClass)
//  private val deleteSymbol = classDenotationClass.getMethod("delete", symbolClass, contextClass)
//  private val invalidateSymbolMemberCaches = classDenotationClass.getMethod("invalidateMemberCaches", contextClass)
  private val emptyScope = emptyScopeClass.getField("MODULE$")
  private val annotationApply = annotationClass.getMethod("apply", typeClass, classOf[List[?]], classOf[Long], contextClass)
  private val annotationApplyWithTree = annotationClass.getMethod("apply", treeClass)
  private val contextFunctionResults = contextFunctionResultsClass.getField("MODULE$")
  private val annotateContextResultsDefDef = contextFunctionResultsClass.getMethod("annotateContextResults", defdefClass, contextClass)

  def createLocalDummy(using Quotes)(symbol: quotes.reflect.Symbol): quotes.reflect.Symbol =
    newLocalDummy.invoke(null, symbol, 0, ctx.invoke(quotes)).asInstanceOf[quotes.reflect.Symbol]

  def setInfo(using Quotes)(symbol: quotes.reflect.Symbol, info: quotes.reflect.TypeRepr): Unit =
    infoSet.invoke(denot.invoke(symbol, ctx.invoke(quotes)), info)

  def enter(using Quotes)(owner: quotes.reflect.Symbol, symbol: quotes.reflect.Symbol): Unit =
    val context = ctx.invoke(quotes)
    val denotation = denot.invoke(owner, context)
    if classDenotationClass.isInstance(denotation) then
      enterSymbol.invoke(denotation, symbol, emptyScope.get(null), context)

  def replace(using Quotes)(owner: quotes.reflect.Symbol, from: quotes.reflect.Symbol, to: quotes.reflect.Symbol): Unit =
    val context = ctx.invoke(quotes)
    val denotation = denot.invoke(owner, context)
    if classDenotationClass.isInstance(denotation) then
      replaceSymbol.invoke(denotation, from, to, context)

//  def invalidateMemberCaches(using Quotes)(symbol: quotes.reflect.Symbol): Unit =
//    val context = ctx.invoke(quotes)
//    val denotation = denot.invoke(symbol, context)
//    if classDenotationClass.isInstance(denotation) then
//      invalidateSymbolMemberCaches.invoke(denotation, context)

  def setFlag(using Quotes)(symbol: quotes.reflect.Symbol, flags: quotes.reflect.Flags) =
    flagSet.invoke(denot.invoke(symbol, ctx.invoke(quotes)), flags)

  def resetFlag(using Quotes)(symbol: quotes.reflect.Symbol, flags: quotes.reflect.Flags) =
    flagReset.invoke(denot.invoke(symbol, ctx.invoke(quotes)), flags)

//  def registerCompanion(using Quotes)(symbol: quotes.reflect.Symbol, companion: quotes.reflect.Symbol) =
//    val context = ctx.invoke(quotes)
//    companionRegister.invoke(denot.invoke(symbol, context), companion, context)

  def updateAnnotation(using Quotes)(symbol: quotes.reflect.Symbol, annotation: quotes.reflect.Symbol, args: List[quotes.reflect.Term]): Unit =
    val context = ctx.invoke(quotes)
    annotationUpdate.invoke(
      denot.invoke(symbol, context),
      annotationApply.invoke(null, annotation.typeRef, args, span.invoke(symbol), context),
      context)

  def updateAnnotationWithTree(using Quotes)(symbol: quotes.reflect.Symbol, tree: quotes.reflect.Tree): Unit =
    val context = ctx.invoke(quotes)
    annotationUpdate.invoke(
      denot.invoke(symbol, context),
      annotationApplyWithTree.invoke(null, tree),
      context)

  def removeAnnotation(using Quotes)(symbol: quotes.reflect.Symbol, annotation: quotes.reflect.Symbol): Unit =
    val context = ctx.invoke(quotes)
    annotationRemove.invoke(denot.invoke(symbol, context), annotation, context)

  def annotateContextResults(using Quotes)(stat: quotes.reflect.DefDef): Unit =
    annotateContextResultsDefDef.invoke(contextFunctionResults.get(null), stat, ctx.invoke(quotes))

object SymbolMutator:
  private val instance =
    try Some(SymbolMutator())
    catch case _: ClassNotFoundException | _ : NoSuchFieldException | _ : NoSuchMethodException => None

  def get: Option[SymbolMutator] = instance

  def getOrErrorAndAbort(using Quotes): SymbolMutator =
    instance.getOrElse(quotes.reflect.report.errorAndAbort("Multitier modules not supported with current compiler version."))
