package retier
package impl
package engine

import scala.reflect.macros.blackbox.Context

object multitier {
  def annotation(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
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

  def setup(c: Context)(peer: c.Expr[Peer]): c.Expr[Runtime] = {
    val processor = CodeProcessor(c)

    def peerConstructionExpressionExpected =
      c.abort(
        c.enclosingPosition,
        "expression of from `new <PeerType> { ... }` expected")

    import c.universe._
    import processor._

    val result = peer.tree match {
      case q"new { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        val q"new { ..$_ } with ..$typedParents { $_ => ..$_ }" =
          processor.c typecheck q"new ..$parents"

        val peerParents = typedParents collect {
          case parent if parent.tpe <:< types.peer => parent.symbol.name
        }

        if (peerParents.isEmpty)
          peerConstructionExpressionExpected

        val tpname = TypeName(peerParents :+ "Instance" mkString "-")
        val tname = TermName("Multitier-Environment")

        q"""
        @${trees.multitier} object $tname {
          class $tpname extends { ..$earlydefns } with ..$parents {
            $self => ..$stats
          }
        }
        multitier.run[$tname.$tpname]
        """

      case _ =>
        peerConstructionExpressionExpected
    }

    processor.c.Expr[Runtime](result)
  }

  def run[P <: Peer: c.WeakTypeTag](c: Context): c.Expr[Runtime] = {
    val processor = CodeProcessor(c)

    import c.universe._
    import processor._

    val peerType = weakTypeOf[P]
    val peerTree = markRetierSynthetic(
      typer createTypeTree peerType, processor.c.enclosingPosition)

    val implementation = peerImplementationTree(peerTree, peerType, List.empty)
    val typeTag = peerTypeTagTree(peerTree, peerType, List.empty)
    val peer = retierTermName("peer")

    import trees._
    import names._

    val result = markRetierSynthetic(
      q"""{
        val $peer = new $peerTree
        $Runtime.run(
          $peer.$connection,
          $peer,
          $typeTag.${names.peerType},
          (context, connections, connected, connecting) => new $implementation {
            override lazy val $system =
              new $System(context, connections, connected, connecting, this)
          }.$system.$systemMain)
      }""",
      peerTree.pos)

    processor.c.Expr[Runtime](result)
  }
}
