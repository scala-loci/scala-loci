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
  private val annotationClass = Class.forName("dotty.tools.dotc.core.Annotations$Annotation")

  private val ctx = quotesImplClass.getMethod("ctx")
  private val denot = symbolClass.getMethod("denot", contextClass)
  private val span = symbolClass.getMethod("span")
  private val infoSet = symDenotationClass.getMethod("info_$eq", typeClass)
  private val annotationUpdate = symDenotationClass.getMethod("updateAnnotation", annotationClass, contextClass)
  private val annotationApply = annotationClass.getMethod("apply", typeClass, classOf[List[_]], classOf[Long], contextClass)

  def setInfo(using Quotes)(symbol: quotes.reflect.Symbol, info: quotes.reflect.TypeRepr): Unit =
    infoSet.invoke(denot.invoke(symbol, ctx.invoke(quotes)), info)

  def updateAnnotation(using Quotes)(symbol: quotes.reflect.Symbol, tpe: quotes.reflect.TypeRepr, args: List[quotes.reflect.Term]): Unit =
    val context = ctx.invoke(quotes)
    val annotation = annotationApply.invoke(null, tpe, args, span.invoke(symbol), context)
    annotationUpdate.invoke(denot.invoke(symbol, context), annotation, context)

object SymbolMutator:
  def make =
    try Some(SymbolMutator())
    catch case _: ClassNotFoundException | _ : NoSuchMethodException => None
