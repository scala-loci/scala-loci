package loci
package language
package impl
package components

import scala.collection.mutable
import scala.reflect.macros.blackbox

object RemoteBlock extends Component.Factory[RemoteBlock](
    requires = Seq(Commons, ModuleInfo, Initialization)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new RemoteBlock(engine)
  def asInstance[C <: blackbox.Context] = { case c: RemoteBlock[C] => c }
}

class RemoteBlock[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq(
    Phase("remote:block", liftRemoteBlocks, after = Set("init:inst"), before = Set("*", "values:collect")))

  val commons = engine.require(Commons)
  val moduleInfo = engine.require(ModuleInfo)
  val initialization = engine.require(Initialization)

  import engine._
  import engine.c.universe._
  import commons._
  import moduleInfo._
  import initialization._

  private val syntheticName = TermName("$loci$synthetic")
  private val placedName = TermName("placed")
  private val callName = TermName("call")
  private val selectName = TermName("select")

  private val placedSymbol = internal.newMethodSymbol(symbols.Placed, placedName, flags = Flag.SYNTHETIC)
  private val callSymbol = internal.newMethodSymbol(symbols.Call, callName, flags = Flag.SYNTHETIC)
  private val selectSymbol = internal.newMethodSymbol(symbols.Select, selectName, flags = Flag.SYNTHETIC)

  private def placed = internal.setSymbol(q"$syntheticName.$placedName", placedSymbol)
  private def call = internal.setSymbol(q"$syntheticName.$callName", callSymbol)
  private def select(selection: List[Tree], selectionType: Tree) = {
    val tree = internal.setSymbol(q"$syntheticName.$selectName[$selectionType](..$selection)", selectSymbol)
    internal.setSymbol(q"$tree.$callName", callSymbol)
  }

  def liftRemoteBlocks(records: List[Any]): List[Any] =
    records process {
      case record @ Initialized(_) =>
        var index = 0
        val liftedDefinitions = mutable.ListBuffer.empty[Tree]

        def liftRemoteBlock(tree: Tree, resultUsed: Boolean) = {
          val q"$expr.$_[..$_](...$exprss)" = tree
          val name = TermName(s"$$loci$$anonymous$$$index")
          index += 1

          // create new symbol for the lifted definition
          val symbol = internal.newMethodSymbol(
            module.classSymbol, name, expr.pos,
            Flag.SYNTHETIC | Flag.PRIVATE | Flag.LOCAL)

          // extract list of captured values
          val (runExpr, captures) = expr match {
            case tree @ q"$expr.$_[..$_](...$exprss)"
              if tree.nonEmpty &&
                 tree.symbol != null &&
                 tree.symbol.owner == symbols.Capture =>
              expr -> (exprss.head collect {
                case tree @ Ident(name: TermName) if isLocalIdentifier(tree.symbol) =>
                  val localType = tree.tpe match {
                    case RefinedType(parents, _) =>
                      parents exists { tpe => isLocalIdentifier(tpe.typeSymbol) }
                    case tpe =>
                      isLocalIdentifier(tpe.typeSymbol)
                  }

                  if (localType)
                    c.abort(tree.pos, "Value of local type cannot be captured")

                  if (!tree.symbol.asTerm.isStable)
                    c.abort(tree.pos, "Stable identifier expected")

                  tree -> internal.setInfo(
                    internal.newTermSymbol(symbol, name, tree.pos, Flag.PARAM),
                    tree.tpe)

                case tree =>
                  c.abort(tree.pos, "Local identifier expected")
              })

            case _ =>
              expr -> List.empty
          }

          // extract list of selected remote references
          val (selection, selectionType) = runExpr match {
            case q"$expr[..$tpts](...$exprss).$_"
                if expr.nonEmpty &&
                   expr.symbol != null &&
                   expr.symbol.owner == symbols.Select =>
              exprss.head -> tpts.head

            case _ =>
              List.empty -> EmptyTree
          }

          // the Scala compiler dealiases the type of the tree
          // generate the canonical type alias
          // (since we check later for that types are in their canonical form)
          val fixedTreeType = {
            val Seq(value, peer) = tree.tpe.underlying.typeArgs

            if (value <:< types.singleSelection)
              types.fromSingle mapArgs { _ => List(value.typeArgs.head, peer) }
            else if (value <:< types.multipleSelection)
              types.fromMultiple mapArgs { _ => List(value.typeArgs.head, peer) }
            else
              types.from mapArgs { _ => List(value, peer) }
          }

          // generate the type of the lifted definition
          // replacing the type of the placed value with `Unit`
          // if the return value is not used
          val tpe = internal.methodType(
            captures map { case (_, symbol) => symbol },
            types.on mapArgs { _ =>
              val Seq(value, peer) = tree.tpe.underlying.typeArgs

              val selected =
                if (value <:< types.singleSelection || value <:< types.multipleSelection)
                  value.typeArgs.head
                else
                  value

              val dispatched =
                if (!resultUsed) {
                  if (selected <:< types.subjective)
                    types.per mapArgs { _ => definitions.UnitTpe :: selected.underlying.typeArgs.tail }
                  else
                    definitions.UnitTpe
                }
                else
                  selected

              List(dispatched, peer)
            })

          internal.setInfo(symbol, tpe)

          // generate the formal parameter list for the lifted definition
          // and the argument list for invoking the definition
          val parameters = captures map { case (tree, symbol) =>
            internal.setSymbol(
              q"${Modifiers(Flag.PARAM)} val ${symbol.name}: ${createTypeTree(tree.tpe, tree.pos)}",
              symbol)
          }

          val arguments = captures map { case (tree, _) => tree }

          // generate the lifted definition
          val result = createTypeTree(tpe.resultType, expr.pos)
          val body = internal.setType(q"$placed(${exprss.head.head})", tpe.resultType)
          val definition = q"${Flag.SYNTHETIC} private[this] def $name(..$parameters): $result = $body"
          internal.setSymbol(definition, symbol)
          internal.setType(definition, tpe)

          val liftedDefinition = transformer transform definition

          liftedDefinitions += atPos(expr.pos) { liftedDefinition }

          // check lifted definition for non-captured local values
          object traverser extends Traverser {
            val localSymbols = (liftedDefinition collect {
              case tree: DefTree if tree.symbol != NoSymbol => tree.symbol
            }).toSet[Symbol]

            val capturedSymbols = (captures map {
              case (tree, _) => tree.symbol
            }).toSet[Symbol]

            override def traverse(tree: Tree): Unit = tree match {
              case tree: RefTree
                  if !tree.symbol.isConstructor &&
                     !(localSymbols contains tree.symbol) &&
                     !(capturedSymbols contains tree.symbol) &&
                     isLocalIdentifier(tree.symbol.owner) =>
                if (tree.symbol.isType)
                  c.abort(tree.pos, "Local type not available inside remote block")
                else
                  c.abort(tree.pos, "Local identifier not available inside remote block; " +
                    "values can be transferred to remote instances using a `run.capture(...)` clause")

              case _ =>
                super.traverse(tree)
            }
          }

          traverser traverse liftedDefinition

          // generate the invocation of the lifted definition
          val lifted = q"${module.className}.this.$name(..$arguments)"
          internal.setSymbol(lifted, symbol)
          internal.setType(lifted, tpe.resultType)

          val liftedCall =
            if (selection.nonEmpty)
              q"${select(selection, selectionType)}($lifted)"
            else
              q"$call($lifted)"

          atPos(expr.pos) { internal.setType(liftedCall, fixedTreeType) }
        }

        def isLocalIdentifier(symbol: Symbol): Boolean =
          symbol != NoSymbol && symbol.owner != NoSymbol && symbol != module.classSymbol &&
          (symbol.isTerm && symbol.owner == module.classSymbol || isLocalIdentifier(symbol.owner))

        def isRemoteBlock(tree: Tree) = tree match {
          case q"$expr[..$_](...$exprss)" =>
            tree.nonEmpty &&
            tree.symbol != null &&
            expr.symbol.owner == symbols.Block
          case _ =>
            false
        }

        object transformer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case q"$expr[..$tpts](...$exprss)"
                if (tree.tpe real_<:< types.remoteAccessor) &&
                   exprss.nonEmpty &&
                   exprss.head.size == 1 &&
                   isRemoteBlock(exprss.head.head) =>
              val lifted = liftRemoteBlock(exprss.head.head, resultUsed = true)
              super.transform(
                atPos(tree.pos) {
                  internal.setType(
                    q"$expr[..$tpts](...${List(lifted) :: exprss.tail})",
                    tree.tpe)
                })

            case _ if isRemoteBlock(tree) =>
              super.transform(liftRemoteBlock(tree, resultUsed = false))

            case _ =>
              super.transform(tree)
          }
        }

        val result = record.copy(tree = record.tree map { (mods, parents, self, body) =>
          (mods, parents, self, (transformer transformTrees body) ++ liftedDefinitions)
        })

        // add the generated lifted definitons to the module's scope
        def classInfoType(parents: List[Type], decls: Scope, typeSymbol: Symbol) = {
          val scope = internal.newScopeWith(decls.toSeq ++ (liftedDefinitions map { _.symbol }): _*)
          internal.classInfoType(parents, scope, typeSymbol)
        }

        val info = module.classSymbol.info match {
          case PolyType(typeParams, ClassInfoType(parents, decls, typeSymbol)) =>
            internal.polyType(typeParams, classInfoType(parents, decls, typeSymbol))
          case ClassInfoType(parents, decls, typeSymbol) =>
            classInfoType(parents, decls, typeSymbol)
        }

        internal.setInfo(module.classSymbol, info)

        result
    }
}
