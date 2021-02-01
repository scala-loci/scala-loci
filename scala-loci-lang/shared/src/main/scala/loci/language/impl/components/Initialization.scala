package loci
package language
package impl
package components

import scala.collection.mutable
import scala.reflect.macros.blackbox

object Initialization extends Component.Factory[Initialization](
    requires = Seq(Commons, ModuleInfo)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new Initialization(engine)
  def asInstance[C <: blackbox.Context] = { case c: Initialization[C] => c }
}

class Initialization[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq(
    Phase("init:inst", instantiateNestedModules, before = Set("*")))

  val commons = engine.require(Commons)
  val moduleInfo = engine.require(ModuleInfo)

  import engine._
  import engine.c.universe._
  import commons._
  import moduleInfo._

  case class Initialized(tree: ImplDef)

  def instantiateNestedModules(records: List[Any]): List[Any] = {
    logging.debug(" Validating the type of the multitier module")

    module.tree.impl.parents foreach { parent =>
      if (!(multitierModules contains parent.symbol) &&
          !isMultitierModule(parent) &&
          parent.tpe =:!= definitions.AnyTpe &&
          parent.tpe =:!= definitions.AnyRefTpe)
        c.abort(parent.pos, "Multitier modules cannot have non-multitier parents")
    }

    def validateSelfType(tree: Tree): Unit = tree match {
      case tq"$tree with ..$trees" =>
        validateSelfType(tree)
        trees foreach validateSelfType

      case _ if tree.tpe != NoType =>
        val symbol = tree.tpe match {
          case RefinedType(Seq(tpe0, tpe1), _) if tpe0.typeSymbol == module.classSymbol =>
            tpe1.typeSymbol
          case tpe =>
            tpe.typeSymbol
        }

        if (!(multitierModules contains symbol) &&
            !isMultitierModule(symbol))
          c.abort(tree.pos, "Multitier modules cannot have non-multitier self-type annotations")

      case _ =>
    }

    validateSelfType(module.tree.impl.self.tpt)

    object transformer extends Transformer {
      sealed trait Level
      case object NoLevel extends Level
      case object ModuleLevel extends Level
      case class ValueLevel(name: String) extends Level

      var level: Level = ModuleLevel

      def withLevel[T](nextLevel: Level)(body: => T) = {
        val prevLevel = level
        level = nextLevel
        val result = body
        level = prevLevel
        result
      }

      override def transform(tree: Tree): Tree = tree match {
        case Block(
              List(classDef @ ClassDef(mods, className, _,_)),
              apply @ Apply(Select(New(Ident(identName)), termNames.CONSTRUCTOR), List()))
            if (mods hasFlag Flag.FINAL) && className == identName =>
          treeCopy.Block(tree, List(transform(classDef)), apply)

        case q"new $expr(...$exprss)" if isMultitierModule(expr) =>
          level match {
            case ValueLevel(name) =>
              val moduleSignature =
                q"""${Flag.SYNTHETIC} protected lazy val $$loci$$sig: ${types.moduleSignature} =
                   ${trees.moduleSignature}(${module.self}.$$loci$$sig, $name)"""

              withLevel(NoLevel) {
                internal.setType(
                  q"new ${super.transform(expr)}(...${exprss map super.transformTrees}) { $moduleSignature }",
                  tree.tpe)
              }
            case _ =>
              withLevel(NoLevel) { super.transform(tree) }
          }

        case tree: Block =>
          treeCopy.Block(tree, withLevel(NoLevel) { transformTrees(tree.stats) }, transform(tree.expr))

        case tree: Apply =>
          withLevel(NoLevel) { super.transform(tree) }

        case tree: ValOrDefDef if level == ModuleLevel =>
          withLevel(ValueLevel(tree.name.toString)) { super.transform(tree) }

        case tree: ImplDef if multitierModules contains tree.symbol =>
          val annotatedTree = tree map { (_, parents, self, body) =>
            val mods = tree.mods mapAnnotations {
              _ filter { _.tpe <:!< types.multitierModule }
            }
            (mods, parents, self, body)
          }
          val name = (tree, level) match {
            case (ModuleDef(_, name, _), ModuleLevel)  => Some(name.toString -> module.self)
            case (ClassDef(_, _, _, _), ValueLevel(name))  => Some(name -> module.self)
            case _ => None
          }

          logging.debug(s" Expanding nested multitier module ${tree.symbol.fullName}")

          val expandedTree = expandMultitierModule(annotatedTree, name)

          logging.debug(" Nested multitier module expanded")
          logging.debug(s" Continuing expansion of multitier module ${module.symbol.fullName}")

          expandedTree

        case tree: ImplDef if tree.symbol != module.symbol && tree.symbol != module.classSymbol =>
          withLevel(NoLevel) { super.transform(tree) }

        case _ =>
          super.transform(tree)
      }
    }

    logging.debug(" Checking nested multitier modules")

    records :+ Initialized((transformer transform module.tree: @unchecked) match { case tree: ImplDef => tree })
  }

  private val multitierModules: Set[Symbol] = {
    val impls = module.tree collect { case tree: ImplDef => tree }
    val multitierModules = mutable.Set(module.symbol)

    var foundAdditionals = true
    while (foundAdditionals) {
      val additionals = (impls collect {
        case impl if isMultitierModule(impl) || (impl.impl.parents exists isMultitierModule) =>
          impl.symbol
      }).toSet -- multitierModules

      if (additionals.isEmpty)
        foundAdditionals = false
      else
        multitierModules ++= additionals
    }

    multitierModules.toSet - module.symbol
  }

  private def isMultitierModule(tree: Tree): Boolean =
    isMultitierModule(tree.symbol)

  private def isMultitierModule(symbol: Symbol): Boolean = {
    symbol == module.symbol ||
    symbol == module.classSymbol ||
    (symbol.allAnnotations exists { _.tree.tpe <:< types.multitierModule })
  }
}
