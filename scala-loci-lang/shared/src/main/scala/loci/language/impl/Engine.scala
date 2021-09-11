package loci
package language
package impl

import scala.reflect.macros.blackbox

trait Engine[C <: blackbox.Context] {
  type Context = c.type

  val c: C
  val components: List[Component[Context]]
  val multitierCode: c.universe.ImplDef
  val outerMultitierCode: c.universe.ImplDef
  val outerMultitierName: List[(String, c.universe.TermName)]

  def require[Comp[Ctx <: blackbox.Context] <: Component[Ctx]](
    factory: Component.Factory[Comp]): Comp[Context]

  def run(code: c.universe.ImplDef, name: Option[(String, c.universe.TermName)]): Engine.Result[Context]
}

object Engine {
  case class Result[C <: blackbox.Context](engine: Engine[C], records: List[Any])

  def run(
      ctx: blackbox.Context)(
      code: ctx.universe.ImplDef,
      factories: Seq[Component.AnyFactory]): Engine.Result[ctx.type] =
    runNested(ctx)(code, code, List.empty, factories)

  private def runNested(
      ctx: blackbox.Context)(
      code: ctx.universe.ImplDef,
      outerCode: ctx.universe.ImplDef,
      outerName: List[(String, ctx.universe.TermName)],
      factories: Seq[Component.AnyFactory]): Engine.Result[ctx.type] = {
    val logging = Logging(ctx)

    val resolved = Components.resolve(factories) match {
      case Components.Resolved(factories) =>
        factories
      case Components.CyclicDependency(cycle) =>
        ctx.abort(ctx.enclosingPosition, s"$initFailed: " +
          s"Cyclic dependencies between components involving ${cycle mkString " -> "}")
    }

    val engine =
      resolved.foldLeft(create(ctx)(code, outerCode, outerName, factories, List.empty)) { (engine, factory) =>
        create(ctx)(code, outerCode, outerName, factories, engine.components :+ factory(engine))
      }

    if (logging.debugEnabled) {
      logging.debug("Multitier components")
      val namedComponents = engine.components map { component =>
        name(component) -> component
      }
      val length = (namedComponents.iterator map { case (componentName, _ ) =>
        componentName.length
      }).max

      namedComponents foreach { case (componentName, component) =>
        val name = s" $componentName"
        val phases = component.phases map { _.name } mkString ", "

        if (phases.isEmpty)
          logging.debug(name)
        else
          logging.debug(s"$name ${" " * (length - componentName.length)} [phases: $phases]")
      }
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

    if (logging.debugEnabled) {
      logging.debug("Multitier expansion phases")
      val length = (phases.iterator map { _.name.length }).max

      phases foreach { phase =>
        val name = s" ${phase.name}"
        val constraints =
          Seq("after: " -> phase.after, "before: " -> phase.before) flatMap {
            case (_, set) if set.isEmpty => None
            case (string, set) => Some(s"$string${set mkString ", "}")
          }

        if (constraints.isEmpty)
          logging.debug(name)
        else
          logging.debug(s"$name ${" " * (length - phase.name.length)} [${constraints mkString "; "}]")
      }
    }

    val codeDumper = CodeDumper(ctx)

    val results =
      phases.foldLeft(Seq.empty[PhaseResult]) { (results, phase) =>
        logging.debug(s"Running multitier expansion phase ${phase.name}")
        val records = results.lastOption.map(_.records).getOrElse(List.empty[Any])
        val result = phase transform records
        val updatedResults = results.appended(PhaseResult(phase.name, result))

        if (codeDumper.isEnabled) {
          codeDumper.dump(
            updatedResults,
            s"${ctx.internal.enclosingOwner.fullName}.${code.name}",
            engine
          )
        }

        updatedResults
      }

    Result(engine, results.lastOption.map(_.records).getOrElse(List.empty[Any]))
  }

  private def create(
      ctx: blackbox.Context)(
      code: ctx.universe.ImplDef,
      outerCode: ctx.universe.ImplDef,
      outerName: List[(String, ctx.universe.TermName)],
      factories: Seq[Component.AnyFactory],
      comps: List[Component[ctx.type]]) =
    new Engine[ctx.type] {
      val c: ctx.type = ctx
      val components = comps
      val multitierCode = code
      val outerMultitierCode = outerCode
      val outerMultitierName = outerName

      def require[Comp[C <: blackbox.Context] <: Component[C]](factory: Component.Factory[Comp]) =
        components collectFirst factory.asInstance getOrElse {
          ctx.abort(ctx.enclosingPosition, s"$initFailed: " +
            s"Required unavailable component ${name(factory)}")
        }

      def run(code: c.universe.ImplDef, name: Option[(String, c.universe.TermName)]) =
        Engine.runNested(ctx)(code, outerMultitierCode, name.toList ++ outerMultitierName, factories)
    }

  private def name(ref: AnyRef) = {
    val name = ref.getClass.getSimpleName
    if (name endsWith "$")
      name.dropRight(1)
    else
      name
  }

  private val initFailed = "Multitier macro engine initialization failed"
}
