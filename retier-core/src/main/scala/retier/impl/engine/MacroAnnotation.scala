package retier
package impl
package engine

import scala.reflect.macros.whitebox.Context

object multitier {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val typer = Typer(c)
    val retierTyper = RetierTyper(c)
    val annottee :: _ = annottees map { _.tree }

    /*
     * The language requires class definitions inside the annottee `A`.
     * References to a nested class `C` in `A` are of type `A.this.C`.
     * Since `A.this.C` refers to the currently expanding annottee `A`,
     * we end up with an "illegal cyclic reference".
     * To work around this issue, we temporarily rename the annottee
     * while processing.
     */
    def renameAnnottee(tree: Tree, newName: Name): Tree = {
      val newTypeName = newName.toTypeName
      val newTermName = newName.toTermName

      class ThisFixer(val originalTypeName: TypeName) extends Transformer {
        override def transform(tree: Tree) = tree match {
          case This(`originalTypeName`) => This(newTypeName)
          case _ => super.transform(tree)
        }
      }

      object ThisFixer {
        def apply(originalName: Name) = new ThisFixer(originalName.toTypeName)
      }

      tree match {
        case ClassDef(mods, name, tparams, impl) =>
          ThisFixer(name) transform ClassDef(mods, newTypeName, tparams, impl)
        case ModuleDef(mods, name, impl) =>
          ThisFixer(name) transform ModuleDef(mods, newTermName, impl)
        case _ =>
          c.abort(tree.pos, "class, trait or module definition expected")
          EmptyTree
      }
    }

    /*
     * Process classes, traits and modules by extracting the statements in the
     * body and wrapping the code into a `CodeWrapper` object.
     */
    val result = annottee match {
      // class or trait definition
      case ClassDef(_, realName, _, _) =>
        val dummyName = TypeName(realName.toString + "$$dummy$$")
        val ClassDef(mods, name, tparams, Template(parents, self, body)) =
          typer typecheck renameAnnottee(annottee, dummyName)

        class ClassWrapper(
            val context: c.type,
            val tree: c.Tree,
            val body: List[c.Tree],
            val bases: List[c.Tree]) extends CodeWrapper[c.type] {

          def replaceBody(body: List[context.Tree]) = {
            val tree = ClassDef(mods, name, tparams, Template(parents, self, body))
            new ClassWrapper(
              context,
              typer retypecheck (retierTyper dropRetierImplicitArguments tree),
              body,
              bases)
          }
        }

        val state = new ClassWrapper(c, annottee, body, parents)
        val result = state

        renameAnnottee(
          typer untypecheck (retierTyper dropRetierImplicitArguments result.tree),
          realName)

      // module definition
      case ModuleDef(_, realName, _) =>
        val dummyName = TypeName(realName.toString + "$$dummy$$")
        val ModuleDef(mods, name, Template(parents, self, body)) =
          typer typecheck renameAnnottee(annottee, dummyName)

        class ModuleWrapper(
            val context: c.type,
            val tree: c.Tree,
            val body: List[c.Tree],
            val bases: List[c.Tree]) extends CodeWrapper[c.type] {

          def replaceBody(body: List[context.Tree]) = {
            val tree = ModuleDef(mods, name, Template(parents, self, body))
            new ModuleWrapper(
              context,
              typer retypecheck (retierTyper dropRetierImplicitArguments tree),
              body,
              bases)
          }
        }

        val state = new ModuleWrapper(c, annottee, body, parents)
        val result = state

        renameAnnottee(
          typer untypecheck (retierTyper dropRetierImplicitArguments result.tree),
          realName)

      case _ =>
        c.abort(
          c.enclosingPosition,
          "`multitier` macro only applicable to class, trait or object")
        EmptyTree
    }

    c.Expr[Any](result)
  }
}
