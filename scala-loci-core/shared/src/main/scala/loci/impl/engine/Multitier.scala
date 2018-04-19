package loci
package impl
package engine

import retypecheck.ReTyper
import scala.reflect.NameTransformer
import scala.reflect.macros.blackbox.Context

object multitier {
  def annotation(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val echo = Echo(c)
    val typer = ReTyper(c)
    val processor = CodeProcessor(c)
    val placedImplicitBridgeCreator = PlacedImplicitBridgeCreator(c)
    val annottee :: companion = annottees map { _.tree }

    import c.universe._
    import placedImplicitBridgeCreator._

    // the current macro expansion always appears twice
    // see: http://stackoverflow.com/a/20466423
    val recursionCount = c.openMacros.count { check =>
      c.enclosingPosition.toString == check.enclosingPosition.toString &&
      c.macroApplication.toString == check.macroApplication.toString
    }
    val isRecursiveExpansion = recursionCount > 2

    val treeCopier = newLazyTreeCopier

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
    val annotteeState = annottee match {
      case ClassDef(_, _, _, _) | ModuleDef(_, _, _)
          if c.hasErrors || isRecursiveExpansion =>
        Left(annottee)

      // class or trait definition
      case ClassDef(_, _, _, Template(_, _, body)) =>
        val constructors = extractConstructors(body)

        class ClassWrapper(val tree: Tree) extends CodeWrapper[c.type] {
          val context: c.type = c
          val ClassDef(mods, tpname, tparams, Template(bases, self, body)) = tree
          val name = tpname

          def transformBody(f: List[context.Tree] => List[context.Tree]) =
            new ClassWrapper(
              treeCopier.ClassDef(tree, mods, tpname, tparams, Template(
                bases, self, constructors ++ extractNonConstructors(f(body)))))

          def typechecked = new ClassWrapper(typer retypecheckAll tree)
          def untypechecked = new ClassWrapper(typer untypecheckAll tree)
        }

        Right(new ClassWrapper(annottee))

      // module definition
      case ModuleDef(_, _, Template(_, _, body)) =>
        val constructors = extractConstructors(body)

        class ModuleWrapper(val tree: Tree) extends CodeWrapper[c.type] {
          val context: c.type = c
          val ModuleDef(mods, tname, Template(bases, self, body)) = tree
          val name = tname.toTypeName

          def transformBody(f: List[context.Tree] => List[context.Tree]) =
            new ModuleWrapper(
              treeCopier.ModuleDef(tree, mods, tname, Template(
                bases, self, constructors ++ extractNonConstructors(f(body)))))

          def typechecked = new ModuleWrapper(typer retypecheckAll tree)
          def untypechecked = new ModuleWrapper(typer untypecheckAll tree)
        }

        Right(new ModuleWrapper(annottee))

      case _ =>
        c.abort(
          c.enclosingPosition,
          "`multitier` macro only applicable to class, trait or object")
    }

    val result = annotteeState match {
      case Left(annotteeState) =>
        annotteeState

      case Right(annotteeState) =>
        val noImplicitBridge = typeOf[Feature#NoImplicitConversionBridge]
        val implicitBridge = (c inferImplicitValue noImplicitBridge).isEmpty

        val state =
          if (implicitBridge)
            annotteeState
            .transformBody { createPlacedImplicitBridges ++ _ }
            .typechecked
            .transformBody { removePlacedImplicitBridges _ }
          else
            annotteeState
            .typechecked

        val name = NameTransformer decode state.tree.symbol.fullName
        echo(verbose = false, s"Expanding `multitier` environment for $name...")

        (processor process state).untypechecked.tree
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
        val parentParents = parents flatMap { _.tpe.baseClasses.tail }
        val peerParents = parents filter { parent =>
          parent.tpe <:< types.peer &&
          !(parentParents contains parent.tpe.typeSymbol)
        }

        peerParents match {
          case Seq() =>
            peerConstructionExpressionExpected

          case Seq(peerParent) =>
            createRuntimeTree(processor)(peerParent.tpe, peerParent, peer.tree)

          case _ =>
            val peerParentNames = peerParents map { _.symbol.name }

            val minus = NameTransformer encode "-"
            val tpname = TypeName(peerParentNames :+ "Instance" mkString minus)
            val tname = TermName(peerParentNames :+ "Setup" mkString minus)

            q"""
            @${trees.multitierAnnotation} ${Flag.SYNTHETIC} object $tname {
              class $tpname extends { ..$earlydefns } with ..$parents {
                $self => ..$stats
              }
            }
            ${trees.multitier}.run[$tname.$tpname]
            """
        }
          
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
    val peerTree = markLociSynthetic(
      typer createTypeTree peerType, processor.c.enclosingPosition)

    processor.c.Expr[Runtime](
      createRuntimeTree(processor)(peerType, peerTree, q"new $peerTree"))
  }

  private def createRuntimeTree[C <: Context](processor: CodeProcessor[C])(
      peerType: processor.c.Type,
      peerTree: processor.c.Tree,
      peerConstructionTree: processor.c.Tree): processor.c.Tree = {
    import processor.c.universe._
    import processor._

    val implementation = peerImplementationTree(peerTree, peerType, List.empty)
    val typeTag = peerTypeTagTree(peerTree, peerType, List.empty)
    val peer = lociTermName("peer")

    import trees._
    import names._

    markLociSynthetic(
      q"""{
        val $peer = $peerConstructionTree
        $Runtime.run(
          $peer.$connection,
          $peer,
          $typeTag.${names.peerType},
          (context, connections, connected, connecting) => new $implementation {
            override def $metapeer = $peer
            override lazy val $system =
              new $System(context, connections, connected, connecting, this)
          }.$system.$systemMain)
      }""",
      peerTree.pos)
  }
}
