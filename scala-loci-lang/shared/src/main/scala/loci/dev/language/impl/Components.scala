package loci.dev
package language
package impl

object Components {
  sealed trait ResolveResult
  case class Resolved(factories: List[Component.AnyFactory]) extends ResolveResult
  case class CyclicDependency(cycle: List[String]) extends ResolveResult

  def resolve(factories: Seq[Component.AnyFactory]): ResolveResult =
    mapResult(expandFactories(factories, List.empty)) { _.distinct }

  private def mapResult(
      result: ResolveResult)(
      f: List[Component.AnyFactory] => List[Component.AnyFactory]): ResolveResult =
    result match {
      case Resolved(factories) => Resolved(f(factories))
      case error => error
    }

  private def expandFactories(
      factories: Seq[Component.AnyFactory],
      path: List[Component.AnyFactory]): ResolveResult = {
    factories.foldLeft[ResolveResult](Resolved(List.empty)) {
      case (Resolved(factories), factory) =>
        mapResult(expandFactory(factory, path)) { factories ++ _ }
      case (error, _) =>
        error
    }
  }

  private def expandFactory(
      factory: Component.AnyFactory,
      path: List[Component.AnyFactory]): ResolveResult =
    if (factory != null && !(path contains factory))
      mapResult(expandFactories(factory.requires, factory :: path)) {
        _ :+ factory
      }
    else
      CyclicDependency(path map { factory =>
        val name = factory.getClass.getSimpleName
        if (name endsWith "$")
          name.substring(0, name.length - 1)
        else
          name
      })
}
