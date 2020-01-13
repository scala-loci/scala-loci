package loci

import contexts.Immediate.Implicits.global

import scribe.{Level, LogRecord, Logger, LoggerSupport, Position}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.experimental.macros
import scala.language.higherKinds
import scala.util.Try
import scala.util.control.NonFatal

package object logging extends LoggerSupport {
  root.orphan().withHandler(minimumLevel = Some(Level.Info)).replace()

  def root = Logger("scala-loci")

  val reportException: Throwable => Unit = exception =>
    error("unhandled exception", tracing(exception))

  def log[M](record: LogRecord[M]): Unit = {
    val logRecord =
      if (record.className startsWith "loci.logging")
        record.copy(className = "scala-loci", methodName = None, line = None, column = None)
      else
        record.copy(className = record.className.replaceAll("\\.\\$anon[^.]*", "").replaceAll("\\.<.*>", ""))

    val logger = Logger.get(logRecord.className) getOrElse {
      Logger().withParent(root).replace(Some(logRecord.className))
    }

    logger.log(logRecord)
  }

  implicit def tracingExecutionContext: TracingExecutionContext =
    macro ImplicitTracingExecutionContext.resolve

  object tracing {
    def apply(context: ExecutionContext): ExecutionContext =
      macro ExplicitTracingExecutionContext.instrument

    def apply[T <: Throwable](throwable: T): T = {
      val stack = throwable.getStackTrace
      val trace =
        ((Position.stack.reverse map {
           pos => pos.copy(className = s"[trace] ${pos.className}").toTraceElement
         }).distinct
         filterNot stack.contains).toArray

      if (trace.nonEmpty) {
        val index = stack lastIndexWhere { element =>
          !(element.getClassName startsWith "java.") &&
          !(element.getClassName startsWith "scala.") &&
          !(element.getClassName startsWith "scribe.") &&
          !(element.getClassName startsWith "loci.logging.") &&
          !(element.getClassName startsWith "loci.contexts.")
        }

        if (index != -1) {
          val (prefix, suffix) = stack splitAt (index + 1)
          throwable.setStackTrace(prefix ++ trace ++ suffix)
        }
        else
          throwable.setStackTrace(stack ++ trace)
      }

      throwable
    }

    def apply[T[U] <: Try[U], U](value: T[U]): T[U] = {
      value.failed foreach { tracing(_) }
      value
    }

    def apply[T](value: Future[T]): Future[T] =
      value recover { case exception => throw tracing(exception) }

    def run[T](body: => T): T =
      try body match {
        case value: Throwable => tracing(value).asInstanceOf[T]
        case value: Try[_] => tracing(value).asInstanceOf[T]
        case value: Future[_] => tracing(value).asInstanceOf[T]
        case value => value
      }
      catch {
        case NonFatal(exception) => throw tracing(exception)
      }
  }
}
