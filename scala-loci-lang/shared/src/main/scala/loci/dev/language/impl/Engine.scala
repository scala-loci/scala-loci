package loci.dev
package language
package impl

import scala.language.higherKinds
import scala.reflect.macros.blackbox

trait Engine[C <: blackbox.Context] {
  type Context = c.type

  val c: C
  val components: List[Component[Context]]
  val multitierCode: c.universe.ImplDef

  def require[Comp[Ctx <: blackbox.Context] <: Component[Ctx]](
    factory: Component.Factory[Comp]): Comp[Context]
}

object Engine {
  case class Result[C <: blackbox.Context](engine: Engine[C], records: List[Any])

  def run(
      ctx: blackbox.Context)(
      code: ctx.universe.ImplDef,
      factories: Seq[Component.AnyFactory]) = {
    val resolved = Components.resolve(factories) match {
      case Components.Resolved(factories) =>
        factories
      case Components.CyclicDependency(cycle) =>
        ctx.abort(ctx.enclosingPosition, s"$initFailed: " +
          s"Cyclic dependencies between components involving ${cycle mkString " -> "}")
    }

    val engine =
      resolved.foldLeft(create(ctx)(code, List.empty)) { (engine, factory) =>
        create(ctx)(code, engine.components :+ factory(engine))
      }

    val phases = Phases.sort(engine.components flatMap { _.phases }) match {
      case Phases.Sorted(phases) =>
        phases
      case Phases.CyclicDependency(cycle) =>
        ctx.abort(ctx.enclosingPosition, s"$initFailed: " +
          s"Cyclic dependencies between phases involving ${cycle mkString " -> "}")
      case Phases.DuplicateIdentifier(name) =>
        ctx.abort(ctx.enclosingPosition, s"$initFailed: " +
          s"Duplicate phase name `$name`")
      case Phases.InvalidIdentifier(name) =>
        ctx.abort(ctx.enclosingPosition, s"$initFailed: " +
          s"Invalid phase name `$name`")
      case Phases.InvalidReference(name) =>
        ctx.abort(ctx.enclosingPosition, s"$initFailed: " +
          s"Dependency to non-existent phase `$name`")
    }

    val result =
      phases.foldLeft(List.empty[Any]) { (list, phase) => phase transform list }

    Result(engine, result)
  }

  private def create(
      ctx: blackbox.Context)(
      code: ctx.universe.ImplDef,
      comps: List[Component[ctx.type]]) =
    new Engine[ctx.type] {
      val c: ctx.type = ctx
      val components = comps
      val multitierCode = code

      def require[Comp[C <: blackbox.Context] <: Component[C]](factory: Component.Factory[Comp]) =
        (components collectFirst factory.asInstance) getOrElse {
          val simpleName = factory.getClass.getSimpleName
          val name =
            if (simpleName endsWith "$")
              simpleName.substring(0, simpleName.length - 1)
            else
              simpleName

          ctx.abort(ctx.enclosingPosition, s"$initFailed: " +
            s"Required unavailable component $name")
        }
    }

  private val initFailed = "Multitier macro engine initialization failed"
}
