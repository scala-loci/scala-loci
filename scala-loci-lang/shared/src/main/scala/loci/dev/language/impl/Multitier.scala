package loci.dev
package language
package impl

import scala.reflect.macros.blackbox

class Multitier(val c: blackbox.Context) {
  import c.universe._

  def annotation(annottees: Tree*): Tree = {
    val annottee :: companion = annottees

    // the current macro expansion always appears twice
    // see: http://stackoverflow.com/a/20466423
    val recursionCount = c.openMacros.count { check =>
      c.enclosingPosition == check.enclosingPosition &&
      c.macroApplication.toString == check.macroApplication.toString
    }
    val isRecursiveExpansion = recursionCount > 2

    val annotteeImpl = annottee match {
      case annottee: ImplDef =>
        annottee
      case _ =>
        c.abort(c.enclosingPosition,
          "multitier annotation only applicable to classes, traits or objects")
    }

    val processedAnnotee: Tree =
      if (!c.hasErrors && !isRecursiveExpansion) {
        import preprocessors._
        import components._
        import retypecheck._

        val retyper = c.retyper

        val preprocessedAnnottee = Preprocessor.run(c)(
          annottee,
          Seq(MultitierTypes))

        val typedAnnottee = retyper typecheck preprocessedAnnottee match {
          case tree: ImplDef => tree
        }

        val Engine.Result(engine, records) = Engine.run(c)(
          typedAnnottee,
          Seq(Commons, ModuleInfo, Peers, Values, Impls, Assembly))

        val assembly = engine.require(Assembly)

        (records
          collectFirst { case assembly.Assembly(annottee) =>
            retyper untypecheckAll annottee
          }
          getOrElse annottee)
      }
      else
        annottee

    (companion.headOption
      map { companion => q"$processedAnnotee; $companion"}
      getOrElse processedAnnotee)
  }
}
