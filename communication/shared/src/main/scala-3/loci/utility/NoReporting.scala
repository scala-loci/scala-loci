package loci
package utility

import java.io.{OutputStream, PrintStream}
import java.util.Locale
import scala.quoted.*

object noReporting:
  def apply[T](default: => T, useExploringContext: Boolean)(body: Quotes ?=> T)(using Quotes): T =
    val invocation = try
      val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
      val quotesImplClass = Class.forName("scala.quoted.runtime.impl.QuotesImpl")
      val quotesImpl = quotesImplClass.getMethod("apply", contextClass)

      Some(
        quotes.getClass.getMethod("ctx").invoke(quotes),
        { (context: Any) =>
          val quotes =
            try quotesImpl.invoke(null, context) match
              case quotes: Quotes => Some(quotes)
              case _ => None
            catch
              case _: IllegalArgumentException =>
                None

          quotes.fold(default) { body(using _) }
        })
    catch
      case _: ClassNotFoundException | _: NoSuchMethodException |  _: IllegalArgumentException =>
        None

    invocation.fold(default) { apply(_)(default, useExploringContext)(_) }
  end apply

  def apply[T](context: Any)(default: => T, useExploringContext: Boolean)(body: Any => T): T =
    val reset = try
      val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
      val freshContextClass = Class.forName("dotty.tools.dotc.core.Contexts$FreshContext")
      val reporterClass = Class.forName("dotty.tools.dotc.reporting.Reporter")
      val noReporterClass = Class.forName("dotty.tools.dotc.reporting.Reporter$NoReporter$")

      val reporter = contextClass.getMethod("reporter")
      val setReporter = freshContextClass.getMethod("setReporter", reporterClass)

      val noReporter = noReporterClass.getField("MODULE$").get(noReporterClass)

      val (freshContext, disposeFreshContext) =
        if useExploringContext then
          try
            val contextsClass = Class.forName("dotty.tools.dotc.core.Contexts")
            val exploreCtx = contextsClass.getMethod("inline$exploreCtx", contextClass)
            val wrapUpExplore = contextsClass.getMethod("inline$wrapUpExplore", contextClass)

            val freshContext = exploreCtx.invoke(null, context)
            freshContext -> { (_: Any) =>
              wrapUpExplore.invoke(null, freshContext)
            }

          catch case _: ClassNotFoundException | _: NoSuchMethodException | _: IllegalArgumentException =>
            val contextStateClass = Class.forName("dotty.tools.dotc.core.Contexts$ContextState")
            val contextPoolClass = Class.forName("dotty.tools.dotc.core.Contexts$ContextPool")
            val exploreContextPool = contextStateClass.getMethod("exploreContextPool")
            val base = contextClass.getMethod("base")
            val next = contextPoolClass.getMethod("next", contextClass)
            val free = contextPoolClass.getMethod("free")

            val pool = exploreContextPool.invoke(base.invoke(context))
            val freshContext = next.invoke(pool, context)
            freshContext -> { (originalReporter: Any) =>
              originalReporter.getClass.getMethod("reset").invoke(originalReporter)
              free.invoke(pool)
            }
        else
          val fresh = contextClass.getMethod("fresh")
          val setExploreTyperState = freshContextClass.getMethod("setExploreTyperState")

          val freshContext = fresh.invoke(context)
          setExploreTyperState.invoke(freshContext)
          freshContext -> { (_: Any) => }
        end if
      end val

      val originalReporter = reporter.invoke(freshContext)
      setReporter.invoke(freshContext, noReporter)

      val finalize = { () =>
        try
          setReporter.invoke(freshContext, originalReporter)
          disposeFreshContext(originalReporter)
        catch
          case _: IllegalArgumentException =>
      }

      Some(freshContext, finalize)

    catch
      case _: ClassNotFoundException | _: NoSuchMethodException | _: NoSuchFieldException | _: IllegalArgumentException =>
        None

    reset.fold(default) { (context, finalize) =>
      try
        val filteredOut = new ProxyPrintStream(Console.out) {
          def filterOut(s: String) = s startsWith "exception"

          override def println(x: String) =
            if !filterOut(x) then super.println(x)

          override def println(x: Any) = x match
            case s: String if filterOut(s) =>
            case _ => super.println(x)
        }

        Console.withOut(filteredOut) {
          body(context)
        }

      finally
        finalize()
    }
  end apply

  private class NullOutputStream extends OutputStream:
    override def write(i: Int): Unit = ()

  private class ProxyPrintStream(stream: PrintStream) extends PrintStream(new NullOutputStream):
    override def flush() = stream.flush()
    override def close() = stream.close()
    override def checkError(): Boolean = stream.checkError()
    override def write(b: Int) = stream.write(b)
    override def write(buf: Array[Byte], off: Int, len: Int) = stream.write(buf, off, len)
    override def print(b: Boolean) = stream.print(b)
    override def print(c: Char) = stream.print(c)
    override def print(i: Int) = stream.print(i)
    override def print(l: Long) = stream.print(l)
    override def print(f: Float) = stream.print(f)
    override def print(d: Double) = stream.print(d)
    override def print(s: Array[Char]) = stream.print(s)
    override def print(s: String) = stream.print(s)
    override def print(obj: Any) = stream.print(obj)
    override def println() = stream.println()
    override def println(x: Boolean) = stream.println(x)
    override def println(x: Char) = stream.println(x)
    override def println(x: Int) = stream.println(x)
    override def println(x: Long) = stream.println(x)
    override def println(x: Float) = stream.println(x)
    override def println(x: Double) = stream.println(x)
    override def println(x: Array[Char]) = stream.println(x)
    override def println(x: String) = stream.println(x)
    override def println(x: Any) = stream.println(x)
    override def printf(format: String, args: Object*) = { stream.printf(format, args); this }
    override def printf(l: Locale, format: String, args: Object*) = { stream.printf(l, format, args); this }
    override def format(format: String, args: Object*) = { stream.format(format, args); this }
    override def format(l: Locale, format: String, args: Object*) = { stream.format(l, format, args); this }
    override def append(csq: CharSequence) = { stream.append(csq); this }
    override def append(csq: CharSequence, start: Int, end: Int) = { stream.append(csq, start, end); this }
    override def append(c: Char) = { stream.append(c); this }
    override def write(b: Array[Byte]) = stream.write(b)
  end ProxyPrintStream
end noReporting
