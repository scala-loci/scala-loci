package loci
package embedding
package impl

import java.io.File
import scala.quoted.*

object APIExtraction:
  def extractAPI(using Quotes)(module: quotes.reflect.ClassDef): Unit =
    import quotes.reflect.*

    val symbol = module.symbol
    val declarations = symbol.declarations.toSet

    module.body foreach:
      case stat: Definition if stat.symbol.owner == symbol && !(declarations contains stat.symbol) =>
        SymbolMutator.getOrErrorAndAbort.enter(symbol, stat.symbol)
      case _ =>

    // workaround for https://github.com/sbt/sbt/issues/7157
    try
      val quotesImplClass = Class.forName("scala.quoted.runtime.impl.QuotesImpl")
      val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
      val treeClass = Class.forName("dotty.tools.dotc.ast.Trees$Tree")
      val extractAPICollectorClass = Class.forName("dotty.tools.dotc.sbt.ExtractAPICollector")
      val classLikeClass = Class.forName("xsbti.api.ClassLike")

      val ctx = quotesImplClass.getMethod("ctx")
      val source = contextClass.getMethod("source")
      val apiSource = extractAPICollectorClass.getMethod("apiSource", treeClass)
      val mainClasses = extractAPICollectorClass.getMethod("mainClasses")

      val incrementalCallbackClass =
        try Class.forName("dotty.tools.dotc.sbt.interfaces.IncrementalCallback")
        catch case _: ClassNotFoundException => null

      val context = ctx.invoke(quotes)
      val sourceFile = source.invoke(context)

      val (api, mainClass, callback, file) =
        if incrementalCallbackClass != null then
          // Scala 3.3.2 (and after)
          val sourceFileClass = Class.forName("dotty.tools.dotc.util.SourceFile")

          val api = incrementalCallbackClass.getMethod("api", sourceFileClass, classLikeClass)
          val mainClass = incrementalCallbackClass.getMethod("mainClass", sourceFileClass, classOf[String])

          val incCallback = contextClass.getMethod("incCallback").invoke(context)

          (api, mainClass, incCallback, sourceFile)
        else
          // Scala 3.3.1 (and before)
          val analysisCallbackClass = Class.forName("xsbti.AnalysisCallback")

          val api = analysisCallbackClass.getMethod("api", classOf[File], classLikeClass)
          val mainClass = analysisCallbackClass.getMethod("mainClass", classOf[File], classOf[String])

          val abstractFile = sourceFile.getClass.getMethod("file").invoke(sourceFile)
          val file = abstractFile.getClass.getMethod("file").invoke(abstractFile)

          val sbtCallback = contextClass.getMethod("sbtCallback").invoke(context)

          (api, mainClass, sbtCallback, file)
        end if

      if callback != null then
        val collector = extractAPICollectorClass.getConstructor(contextClass).newInstance(context)

        apiSource.invoke(collector, module) match
          case classes: Iterable[?] => classes foreach { api.invoke(callback, file, _) }
          case _ =>

        mainClasses.invoke(collector) match
          case mainClasses: Iterable[?] => mainClasses foreach { mainClass.invoke(callback, file, _) }
          case _ =>
      end if
    catch
      case _: ClassNotFoundException | _ : NoSuchMethodException =>
  end extractAPI
end APIExtraction
