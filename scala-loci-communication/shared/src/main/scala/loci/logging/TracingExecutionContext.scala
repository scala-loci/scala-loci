package loci
package logging

import scribe.{LoggingExecutionContext, Position}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.reflect.macros.whitebox

class TracingExecutionContext(context: ExecutionContext, stack: List[Position]) extends
    LoggingExecutionContext(context, stack) with
    ExecutionContextExecutor {
  override def execute(runnable: Runnable) = super.execute(new Runnable {
    override def run() = tracing run { runnable.run() }
  })
}

object ImplicitTracingExecutionContext {
  def resolve(c: whitebox.Context): c.Tree = {
    import c.universe._

    if (c.hasErrors)
      c.abort(c.enclosingPosition, "Skipping tracing execution context macro due to compilation errors")

    // the current macro expansion always appears twice
    // see: http://stackoverflow.com/a/20466423
    val recursionCount = c.openMacros.count { other =>
      c.enclosingPosition == other.enclosingPosition &&
        c.macroApplication.toString == other.macroApplication.toString
    }
    if (recursionCount > 2)
      c.abort(c.enclosingPosition, "Skipping tracing execution context macro for recursive invocation")

    val tree = c.inferImplicitValue(typeOf[ExecutionContext])
    if (tree.isEmpty)
      c.abort(c.enclosingPosition, "Skipping tracing execution context macro due to unresolved execution context")

    // flag symbol of inferred tree as synthetic if it is private or a local variable
    // to prevent "value is never used" warnings
    val symbol = tree.symbol.asTerm
    if (!symbol.isGetter && !symbol.isParamAccessor || symbol.isGetter && symbol.isPrivate)
      c.internal.setFlag(tree.symbol, Flag.SYNTHETIC)

    ExplicitTracingExecutionContext.instrument(c)(tree)
  }
}

object ExplicitTracingExecutionContext {
  def instrument(c: whitebox.Context)(context: c.Tree): c.Tree = {
    import c.universe._

    val stack = c.typecheck(q"${termNames.ROOTPKG}.scribe.Execution.custom(null)", silent = true) match {
      case q"new $_($_, $stack)" =>
        Right(stack)
      case q"new $_($_, $stack): $_" =>
        Right(stack)
      case tree =>
        Left(tree)
    }

    val tracingContext = tq"${termNames.ROOTPKG}.loci.logging.TracingExecutionContext"

    stack match {
      case Right(stack) =>
        q"new $tracingContext($context, $stack)"

      case Left(tree) =>
        val message = s"scribe logging framework custom execution context macro generated unexpected code: $tree"
        q"""{
          @${termNames.ROOTPKG}.scala.annotation.compileTimeOnly($message) def unexpectedTree() = ()
          unexpectedTree()
          new $tracingContext(null, null)
        }"""
    }
  }
}
