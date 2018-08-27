package loci.dev
package language
package impl

import scala.reflect.macros.blackbox

class Multitier(val c: blackbox.Context) extends MultitierCode {
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

    val code = annottee match {
      case ClassDef(_, _, _, _) | ModuleDef(_, _, _)
          if c.hasErrors || isRecursiveExpansion =>
        Left(annottee)
      case ClassDef(_, _, _, Template(_, _, _)) =>
        Right(new MultitierClass(annottee))
      case ModuleDef(_, _, Template(_, _, _)) =>
        Right(new MultitierModule(annottee))
      case _ =>
        c.abort(
          c.enclosingPosition,
          "multitier annotation only applicable to classes, traits or objects")
    }

    val result = code match {
      case Left(annottee) =>
        annottee

      case Right(untypedCode) =>
        import components._

        val code = untypedCode.typechecked

        val Engine.Result(engine, records) =
          Engine.run(c)(code.tree, Seq(Commons, ModuleInfo, Peers, Values, Assembly))

        val values = engine.require(Values)

        val result = code replaceBody
          (records collect { case values.GlobalValue(_, _, tree) => tree })

        result.untypechecked.tree
    }

    companion.headOption map { companion => q"$result; $companion"} getOrElse result
  }
}
