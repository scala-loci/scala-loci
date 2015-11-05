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
    val annottee :: companion = annottees map { _.tree }

    /*
     * The language requires class definitions inside the annottee `A`.
     * References to a nested class `C` in `A` are of type `A.this.C`.
     * Since `A.this.C` refers to the currently expanding annottee `A`,
     * we end up with an "illegal cyclic reference".
     * To work around this issue, we temporarily rename the annottee
     * while processing.
     */
    def renameAnnottee(tree: Tree, name: Name): Tree = {
      val typeName = name.toTypeName
      val termName = name.toTermName

      class SelfReferenceChanger(originalName: Name) extends Transformer {
        val originalTypeName = originalName.toTypeName
        val originalTermName = originalName.toTermName

        override def transform(tree: Tree) = tree match {
          case tree: TypeTree if tree.original != null =>
            internal setOriginal (tree, transform(tree.original))
          case ClassDef(_, `originalTypeName`, _, _) => tree
          case ModuleDef(_, `originalTermName`, _) => tree
          case This(`originalTypeName`) => This(typeName)
          case _ => super.transform(tree)
        }
      }

      tree match {
        case ClassDef(mods, name, tparams, impl) =>
          new SelfReferenceChanger(name) transform
            ClassDef(mods, typeName, tparams, impl)
        case ModuleDef(mods, name, impl) =>
          new SelfReferenceChanger(name) transform
            ModuleDef(mods, termName, impl)
        case _ =>
          c.abort(tree.pos, "class, trait or module definition expected")
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
      case ClassDef(_, originalName, _, _) =>
        val surrogateName = typer surrogateName originalName
        val ClassDef(mods, tpname, tparams, Template(parents, self, body)) =
          renameAnnottee(annottee, surrogateName)
        val constructors = extractConstructors(body)

        class ClassWrapper(stats: List[c.Tree]) extends CodeWrapper[c.type] {
          val context: c.type = c
          val tree =
            typer retypecheckAll
              ClassDef(mods, tpname, tparams, Template(
                parents, self, constructors ++ extractNonConstructors(stats)))
          val name = surrogateName
          val  ClassDef(_, _, _, Template(bases, _, body)) = tree

          def replaceBody(body: List[context.Tree]) = new ClassWrapper(body)
        }

        val state = new ClassWrapper(body)

        val name = state.tree.symbol.fullName
        echo(verbose = false, s"Expanding `multitier` environment for $name...")

        val result = processor process state

        renameAnnottee(typer untypecheckAll result.tree, originalName)

      // module definition
      case ModuleDef(_, originalName, _) =>
        val surrogateName = typer surrogateName originalName.toTypeName
        val ModuleDef(mods, tname, Template(parents, self, body)) =
          renameAnnottee(annottee, surrogateName)
        val constructors = extractConstructors(body)

        class ModuleWrapper(stats: List[c.Tree]) extends CodeWrapper[c.type] {
          val context: c.type = c
          val tree =
            typer retypecheckAll
              ModuleDef(mods, tname, Template(
                parents, self, constructors ++ extractNonConstructors(stats)))
          val name = surrogateName
          val ModuleDef(_, _, Template(bases, _, body)) = tree

          def replaceBody(body: List[context.Tree]) = new ModuleWrapper(body)
        }

        val state = new ModuleWrapper(body)

        val name = state.tree.symbol.fullName
        echo(verbose = false, s"Expanding `multitier` environment for $name...")

        val result = processor process state

        renameAnnottee(typer untypecheckAll result.tree, originalName)

      case _ =>
        c.abort(
          c.enclosingPosition,
          "`multitier` macro only applicable to class, trait or object")
    }

    if (companion.isEmpty)
      c.Expr[Any](result)
    else
      c.Expr[Any](q"$result; ${companion.head}")
  }
}
