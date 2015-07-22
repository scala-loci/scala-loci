package retier
package impl
package engine

import scala.reflect.macros.whitebox.Context

object multitier {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val echo = Echo(c)
    val typer = Typer(c)
    val processor = CodeProcessor(c)
    val annottee :: _ = annottees map { _.tree }

    echo(verbose = false, "Expanding `multitier` environment")

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

      class SelfReferenceChanger(val originalName: Name) extends Transformer {
        val original = originalName.encodedName.toString

        override def transform(tree: Tree) = tree match {
          case ClassDef(_, name, _, _)
            if name.encodedName.toString == original => tree
          case This(name)
            if name.encodedName.toString == original => This(newTypeName)
          case _ => super.transform(tree)
        }
      }

      tree match {
        case ClassDef(mods, name, tparams, impl) =>
          new SelfReferenceChanger(name) transform
            ClassDef(mods, newTypeName, tparams, impl)
        case ModuleDef(mods, name, impl) =>
          new SelfReferenceChanger(name) transform
            ModuleDef(mods, newTermName, impl)
        case _ =>
          c.abort(tree.pos, "class, trait or module definition expected")
          EmptyTree
      }
    }

    def extractConstructors(tree: List[c.Tree]): List[c.Tree] =
      tree filter {
        case DefDef(_, termNames.CONSTRUCTOR, _, _, _, _) => true
        case _ => false
      }

    def extractNonConstructors(tree: List[c.Tree]): List[c.Tree] =
      tree filterNot {
        case DefDef(_, termNames.CONSTRUCTOR, _, _, _, _) => true
        case _ => false
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
          renameAnnottee(annottee, dummyName)
        val constructors = extractConstructors(body)

        class ClassWrapper(stats: List[c.Tree]) extends CodeWrapper[c.type] {
          val context: c.type = c
          val bases = parents
          val tree =
            typer retypecheckAll
              ClassDef(mods, name, tparams, Template(
                parents, self, constructors ++ extractNonConstructors(stats)))
          val body = tree.asInstanceOf[ClassDef].impl.body

          def replaceBody(body: List[context.Tree]) = new ClassWrapper(body)
        }

        val state = new ClassWrapper(body)
        val result = processor process state

        renameAnnottee(typer untypecheckAll result.tree, realName)

      // module definition
      case ModuleDef(_, realName, _) =>
        val dummyName = TypeName(realName.toString + "$$dummy$$")
        val ModuleDef(mods, name, Template(parents, self, body)) =
          renameAnnottee(annottee, dummyName)
        val constructors = extractConstructors(body)

        class ModuleWrapper(stats: List[c.Tree]) extends CodeWrapper[c.type] {
          val context: c.type = c
          val bases = parents
          val tree =
            typer retypecheckAll
              ModuleDef(mods, name, Template(
                parents, self, constructors ++ extractNonConstructors(stats)))
          val body = tree.asInstanceOf[ModuleDef].impl.body

          def replaceBody(body: List[context.Tree]) = new ModuleWrapper(body)
        }

        val state = new ModuleWrapper(body)
        val result = processor process state

        renameAnnottee(typer untypecheckAll result.tree, realName)

      case _ =>
        c.abort(
          c.enclosingPosition,
          "`multitier` macro only applicable to class, trait or object")
        EmptyTree
    }

    c.Expr[Any](result)
  }
}
