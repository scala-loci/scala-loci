package loci
package language
package impl

import retypecheck._

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.reflect.NameTransformer
import scala.reflect.macros.blackbox

class Instance(val c: blackbox.Context) {
  import c.universe._

  object types {
    val on = typeOf[_ on _]
    val per = typeOf[_ per _ ]
    val instance = typeOf[loci.Instance[_]]
    val system = typeOf[runtime.System]
    val abstractValue = typeOf[runtime.AbstractValue]
    val multitierModule = typeOf[runtime.MultitierModule]
    val connections = typeOf[language.Connections]
    val executionContext = typeOf[ExecutionContext]
    val placedValues = c.mirror.staticClass("_root_.loci.runtime.PlacedValues").asType.toType
  }

  object symbols {
    val lifts = (symbolOf[Placed[_, _]].companion.info member TermName("lift")).alternatives
  }

  object names {
    def placedValues(symbol: Symbol) =
      TypeName(NameTransformer encode s"<placed values of ${uniqueRealisticName(symbol)}>")

    private def uniqueRealisticName(symbol: Symbol): String = {
      val owner = symbol.owner
      val name = symbol.name.toString

      val realisticName =
        if (name startsWith "$loci$multitier$")
          (symbol.owner.info member TermName(name.drop(16)) orElse symbol).name.toString
        else
          name

      if (owner == c.mirror.RootClass)
        realisticName
      else if (symbol.isSynthetic || ((realisticName startsWith "<") && (realisticName endsWith ">")))
        uniqueRealisticName(owner)
      else {
        val prefix = uniqueRealisticName(owner)
        val separator = if (owner.isType && !owner.isModuleClass) "#" else "."
        val suffix =
          if (realisticName endsWith termNames.LOCAL_SUFFIX_STRING)
            realisticName.dropRight(termNames.LOCAL_SUFFIX_STRING.length)
          else
            realisticName
        s"$prefix$separator$suffix"
      }
    }
  }

  def allAnnotations(symbol: Symbol): List[Annotation] =
    if (symbol.isMethod && symbol.asMethod.isAccessor)
      symbol.annotations ++ symbol.asMethod.accessed.annotations
    else
      symbol.annotations

  def asSeenFrom(tpe: Type, prefix: Type): Type = {
    val symbol = prefix.typeSymbol
    val bases =
      if (symbol.isClass)
        symbol.asClass.selfType.baseClasses.toSet
      else
        symbol.info.baseClasses.toSet

    val symbols = mutable.ListBuffer.empty[Symbol]

    tpe foreach {
      case TypeRef(_, sym, _) => symbols += sym.owner
      case ThisType(sym) => symbols += sym
      case SingleType(_, sym) if sym.isModule => symbols += sym.asModule.moduleClass
      case SingleType(_, sym) if sym.isModuleClass => symbols += sym
      case _ =>
    }

    symbols.foldLeft(tpe) { (tpe, symbol) =>
      if (bases contains symbol)
        tpe.asSeenFrom(prefix, symbol)
      else
        tpe
    }
  }

  val documentationCompiler =
    c.compilerSettings.size > 1 && (c.compilerSettings sliding 2 exists {
      case Seq(flag, value) =>
        flag == "-d" && ((value endsWith "/api") || (value endsWith "\\api"))
    })

  def retrieve(retrievable: Tree): Tree =
    c.macroApplication match {
      case _ if documentationCompiler =>
        q"${termNames.ROOTPKG}.scala.Predef.???"

      case q"$_[..$_]($instance).$_[$_]($_[..$_]($expr))" =>
        val pos = if (expr.pos != NoPosition) expr.pos else c.enclosingPosition

        expr match {
          case q"$prefix.$name[..$tpt](...$exprss)" =>
            val symbol = prefix.tpe.typeSymbol

            // force loading of annotations
            symbol.info

            if (!(allAnnotations(symbol) exists { _.tree.tpe <:< types.multitierModule }))
              c.abort(
                if (prefix.pos != NoPosition) prefix.pos else pos,
                s"$prefix is not a multitier module")

            val (instancePeer, instanceModule) = instance.tpe.widen.typeArgs.head match {
              case tpe @ TypeRef(pre, _, _) =>
                tpe -> pre.termSymbol
              case tpe =>
                val symbol = tpe.typeSymbol.owner
                if (symbol.isModuleClass)
                  tpe -> symbol.asClass.module
                else
                  tpe -> symbol
            }

            def splitPrefixPath(tree: Tree): (Tree, Tree) = tree match {
              case _ if tree.symbol == instanceModule =>
                tree -> atPos(tree.pos) { Ident(TermName("values")) }
              case Select(qualifier, name) =>
                val (placedPrefix, placedPath) = splitPrefixPath(qualifier)
                placedPrefix -> atPos(tree.pos) { Select(placedPath, name) }
              case Apply(fun, args) =>
                val (placedPrefix, placedPath) = splitPrefixPath(fun)
                placedPrefix -> atPos(tree.pos) { Apply(placedPath, args) }
              case TypeApply(fun, args) =>
                val (placedPrefix, placedPath) = splitPrefixPath(fun)
                placedPrefix -> atPos(tree.pos) { TypeApply(placedPath, args) }
              case _ =>
                c.abort(pos, s"$prefix.$name is not a value of multitier module ${instanceModule.fullName}")
            }

            val (placedPrefix, placedPath) = splitPrefixPath(prefix)

            val Seq(value, peer) =
              if (expr.tpe <:< types.on)
                expr.tpe.widen.typeArgs
              else
                Seq(expr.tpe, NoType)

            if (peer != NoType && !(instancePeer <:< peer))
              c.abort(pos, s"$expr is not placed on $peer")

            object transformer extends Transformer {
              override def transform(tree: Tree): Tree = tree match {
                case q"$_[..$_](...$exprss)"
                    if tree.nonEmpty && (symbols.lifts contains tree.symbol) =>
                  exprss.head.head

                case _ =>
                  super.transform(tree)
              }
            }

            val access =
              if (peer != NoType && value <:< types.per) {
                val Seq(subjective, remote) = value.widen.typeArgs
                val access =
                  if (expr.symbol.asTerm.isStable)
                    q"$placedPath.$$loci$$sys.subjectiveValue($placedPath.$name[..$tpt](...$exprss), remote)"
                  else
                    q"$placedPath.$name[..$tpt](...$exprss)(remote)"

                q"""new ${termNames.ROOTPKG}.loci.Instance.SubjectiveValue[$subjective, $remote] {
                  def to(remote: ${termNames.ROOTPKG}.loci.Remote[$remote]) = $access
                }"""
              }
              else
                q"$placedPath.$name[..$tpt](...$exprss)"

            transformer transform q"""$instance match {
              case instance: ${termNames.ROOTPKG}.loci.runtime.Instance[_] => instance.values match {
                case values: $placedPrefix.${names.placedValues(instanceModule.info.typeSymbol)} => $access
                case _ => throw new ${termNames.ROOTPKG}.loci.runtime.PeerImplementationError
              }
              case _ => throw new ${termNames.ROOTPKG}.loci.runtime.PeerImplementationError
            }"""

          case _ =>
            c.abort(pos, "Peer value expected: <module>.<value>")
        }

      case _ =>
        c.abort(c.enclosingPosition,
          "Access to peer values must be of the form: <peer instance of module> retrieve <module>.<value>")
    }

  def start(instance: Tree): Tree = {
    // parse named arguments for constructor invocation
    val (stats, expr) = instance match {
      case Block(stats @ List(ClassDef(_, _, _, _)), expr) =>
        List.empty -> instance

      case Block(stats, expr) =>
        val definitions = stats.foldRight(Option(List.empty[ValDef])) {
          case (tree @ ValDef(mods, name, _, rhs), stats) if tree.mods hasFlag Flag.ARTIFACT =>
            stats map { treeCopy.ValDef(tree, mods, name, TypeTree(), rhs) :: _ }
          case _ =>
            None
        }
        definitions map { _ -> expr } getOrElse List.empty -> instance

      case _ =>
        List.empty -> instance
    }

    expr match {
      case _ if documentationCompiler =>
        q"${termNames.ROOTPKG}.scala.Predef.???"

      case q"new { ..$earlydefs } with $inst[$tpt](...$exprss) { $self => ..$body }"
          if inst.tpe <:< types.instance.typeConstructor =>

        val pos = if (tpt.pos != NoPosition) tpt.pos else c.enclosingPosition

        if (earlydefs.nonEmpty)
          c.abort(earlydefs.head.pos,
            "Early initialization not permitted (note that values are automatically initialized early if possible)")

        // create prefix path for multitier module
        val peer = TypeName(s"$$loci$$peer$$${tpt.symbol.name}")
        val signature = TermName(s"$$loci$$peer$$sig$$${tpt.symbol.name}")
        val ties = TermName(s"$$loci$$peer$$ties$$${tpt.symbol.name}")

        val (prefix, placedValues) = tpt match {
          case tq"$prefix.$_" =>
            prefix -> (prefix.tpe member peer)
          case _ =>
            EmptyTree -> NoSymbol
        }

        if (prefix.nonEmpty) {
          val symbol = prefix.tpe.typeSymbol

          // force loading of annotations
          symbol.info

          def hasMultitierBase(symbol: Symbol): Boolean =
            (allAnnotations(symbol) exists { _.tree.tpe <:< types.multitierModule }) ||
            symbol != NoSymbol && hasMultitierBase(symbol.owner)

          if (!(allAnnotations(symbol) exists { _.tree.tpe <:< types.multitierModule }))
            c.abort(pos, s"$prefix is not a multitier module")

          if (!symbol.isModule && !symbol.isModuleClass && !symbol.isFinal)
            c.abort(pos, s"$prefix is not a valid multitier module instance, " +
              "i.e., an object or an instance of a final class")

          if (hasMultitierBase(symbol.owner))
            c.abort(pos, s"cannot instantiate peer of nested multitier module $prefix")
        }

        if (!placedValues.isType)
          c.abort(pos, s"$tpt is not a peer type")

        // parse named arguments for constructor invocation of anonymous class
        val (args, exprs) =
          if (exprss.isEmpty) {
            val args = expr collect {
              case tree: DefDef
                  if tree.symbol.isConstructor && tree.symbol.owner == expr.tpe.typeSymbol =>
                tree.rhs collect {
                  case tree @ ValDef(mods, name, _, rhs) if tree.mods hasFlag Flag.ARTIFACT =>
                    Left(treeCopy.ValDef(tree, mods, name, TypeTree(), rhs))
                  case Apply(Select(Super(_, _), termNames.CONSTRUCTOR), args) =>
                    Right(args)
                }
            }

            if (args.nonEmpty)
              (args.head collect { case Left(tree) => tree }) ->
                (args.head collectFirst { case Right(args) => args }).toList.flatten
            else
              List.empty -> List.empty
          }
          else
            stats -> exprss.head

        // collect required named arguments
        val namedArgs = args filter { tree =>
          exprs exists {
            case Ident(name) => name == tree.name
            case _ => false
          }
        }

        // extract peer instance setup from arguments
        val separateMainThread = (exprs
          collectFirst { case arg if arg.tpe <:< definitions.BooleanTpe => arg }
          getOrElse q"true")

        val context = (exprs
          collectFirst { case arg if arg.tpe <:< types.executionContext => arg }
          getOrElse q"${termNames.ROOTPKG}.loci.contexts.Queued.create")

        val connect = (exprs
          collectFirst { case arg if arg.tpe <:< types.connections => arg }
          getOrElse q"${termNames.ROOTPKG}.loci.language.Connections.empty")

        // construct peer instance
        val (earlyDefinitions, lateDefinitions) =
          collectDefinitions(
            prefix.tpe,
            tpt.tpe,
            placedValues.asType.toType.asSeenFrom(prefix.tpe, prefix.tpe.typeSymbol),
            body,
            q"$$loci$$module",
            tpt.pos)

        val definedClasses = (instance collect {
          case tree: DefTree if tree.symbol.isClass => tree.symbol
          case tree: DefTree if tree.symbol.isModule => tree.symbol.asModule.moduleClass
        }).toSet

        object transformer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case Select(qualifier @ This(_), name) if definedClasses contains qualifier.symbol =>
              Ident(name)
            case _ if tree.tpe != null && (tree.tpe exists { _ <:< types.instance }) =>
              super.transform(internal.setType(tree, null))
            case _ =>
              super.transform(tree)
          }
        }

        val main =
          if ((prefix.tpe member TermName("main")).alternatives exists { symbol =>
              !symbol.isPrivate &&
              !symbol.isProtected &&
              symbol.privateWithin == NoSymbol &&
              symbol.isMethod &&
              !symbol.asMethod.isAccessor && {
                val paramLists = symbol.asMethod.paramLists
                paramLists.isEmpty || paramLists.size == 1 && paramLists.head.isEmpty
              } && {
                val tpe = asSeenFrom(symbol.info.finalResultType.widen, prefix.tpe)
                !(tpe <:< definitions.NothingTpe) &&
                !(tpe <:< definitions.NullTpe) &&
                (!(tpe <:< types.on) || tpt.tpe <:< tpe.typeArgs.last)
              }
            })
            q"${termNames.ROOTPKG}.scala.Some(main _)"
          else
            q"${termNames.ROOTPKG}.scala.None"

        val system =
          q"""${Flag.SYNTHETIC} protected def $$loci$$sys$$create = new ${types.system}(
             this, $main, $separateMainThread,
             $$loci$$ties, $$loci$$context, $$loci$$connections, $$loci$$connected, $$loci$$connecting)"""

        val tree = transformer transform q"""
          ..$namedArgs
          ${termNames.ROOTPKG}.loci.runtime.Runtime.start(
            $prefix.$signature,
            $prefix.$ties,
            $context,
            $connect.setup($prefix.$signature, $prefix.$ties.keys.flatMap(_.bases).toList),
            ($$loci$$ties, $$loci$$context, $$loci$$connections, $$loci$$connected, $$loci$$connecting) => {
              val $$loci$$module: $prefix.type = $prefix
              val $$loci$$instance =
                new { ..$earlyDefinitions } with $prefix.$peer { $$loci$$outer =>
                  $system
                  ..$lateDefinitions
                }
              $$loci$$instance.$$loci$$sys.start()
              $$loci$$instance
            })"""

        c.retyper untypecheckAll tree

      case _ =>
        c.abort(c.enclosingPosition,
          "Multitier peer instantiation must be of the form: multitier start new Instance[P]")
    }
  }

  private def collectDefinitions(
      module: Type,
      peer: Type,
      placedValues: Type,
      stats: List[Tree],
      path: Tree,
      pos: Position): (List[Tree], List[Tree]) = {

    // collect value definitions
    def methodShapeType(tpe: Type): Type = {
      def methodShapeType(tpe: Type): Type = tpe match {
        case PolyType(typeParams, resultType) =>
          internal.polyType(typeParams, methodShapeType(resultType))
        case MethodType(params, resultType) =>
          internal.methodType(params, methodShapeType(resultType))
        case _ =>
          definitions.UnitTpe
      }

      tpe match {
        case MethodType(List(), MethodType(_, _)) => methodShapeType(tpe)
        case MethodType(List(), resultType) => definitions.UnitTpe
        case NullaryMethodType(tpe) => definitions.UnitTpe
        case _ => methodShapeType(tpe)
      }
    }

    val valueDefinitions = stats map {
      case tree: ValDef =>
        (tree, tree.symbol.asTerm, tree.name, methodShapeType(tree.symbol.info))

      case tree: DefDef =>
        (tree, tree.symbol.asTerm, tree.name, methodShapeType(tree.symbol.info))

      case tree: ModuleDef =>
        val q"$mods object $name extends { ..$earlydefs } with ..$parents { $self => ..$body }" = tree

        val symbol = module member name
        val multitierModuleType =
          if (symbol != NoSymbol &&
              symbol.asTerm.isStable &&
              symbol.asTerm.isLazy &&
              symbol.isFinal) {
            val typeSymbol = symbol.info.typeSymbol
            if (typeSymbol.isModuleClass &&
                (allAnnotations(typeSymbol) exists { _.tree.tpe <:< types.multitierModule }))
              typeSymbol.asType.toType.asSeenFrom(module, module.typeSymbol)
            else
              NoType
          }
          else
            NoType

        val placedValues = multitierModuleType member names.placedValues(multitierModuleType.typeSymbol)

        if (placedValues.isType) {
          if (self.name != termNames.WILDCARD || self.tpt.tpe != NoType)
            c.abort(self.pos,
              "Self-type annotation not permitted")
          if (parents exists { tree => !(tree.tpe =:= definitions.AnyTpe || tree.tpe =:= definitions.AnyRefTpe) })
            c.abort(parents.head.pos,
              "Inheritance not permitted")
          if (earlydefs.nonEmpty)
            c.abort(earlydefs.head.pos,
              "Early initialization not permitted (note that values are automatically initialized early if possible)")

          val multitierName = TermName(s"$$loci$$multitier$$$name")

          val multitierPeerParents = (multitierModuleType.members
            collect {
              case symbol
                  if symbol.isAbstract &&
                     symbol.isType &&
                     !symbol.isClass &&
                     peer <:< symbol.asType.toType =>
                val peerName = TypeName(s"$$loci$$peer$$${symbol.name}")
                val peer = multitierModuleType member peerName
                if (peer.isType)
                  Some(tq"$path.$name.$peerName")
                else
                  None
            }).flatten.toList

          val multitierParents =
            if (multitierPeerParents.nonEmpty)
              multitierPeerParents
            else
              List(tq"$path.$name.${placedValues.asType.name}")

          val (earlyDefinitions, lateDefinitions) =
            collectDefinitions(
              multitierModuleType,
              peer,
              internal.typeRef(multitierModuleType, placedValues, List.empty),
              body,
              q"$path.$name",
              tree.pos)

          val system = q"""${Flag.SYNTHETIC} protected def $$loci$$sys$$create =
            $$loci$$outer.$$loci$$sys"""

          val multitierModule = atPos(tree.pos) {
            q"""${Flag.SYNTHETIC} protected[this] def $multitierName: $path.$name.${placedValues.asType.name} =
              new { ..$earlyDefinitions } with ..$multitierParents { $self =>
                $system
                ..$lateDefinitions
              }"""
          }

          (multitierModule, tree.symbol.asTerm, name, methodShapeType(tree.symbol.info))
        }
        else
          (tree, tree.symbol.asTerm, name, methodShapeType(tree.symbol.info))

      case tree =>
        c.abort(tree.pos, "Only definitions allowed in multitier peer instantiation")
    }

    valueDefinitions foreach { case (_, symbol, _, _) =>
      val getter = if (symbol.getter != NoSymbol) symbol.getter.asTerm else symbol

      val scope =
        if (getter.privateWithin != NoSymbol)
          s"[${getter.privateWithin.name}]"
        else
          ""

      val visibility =
        if (getter.isPrivateThis && (getter.isModule || getter.isLazy))
          "private[this]"
        else if (getter.isPrivate && !getter.isPrivateThis)
          "private"
        else if (getter.isProtectedThis)
          "protected[this]"
        else if (getter.isProtected)
          s"protected$scope"
        else if (scope.nonEmpty)
          s"private$scope"
        else
          ""

      if (visibility.nonEmpty)
        c.abort(getter.pos, s"Visibility modifier not permitted: $visibility")
    }

    // check that value definitions conform (potentially) overridden definitions
    // and make sure abstract definitions are implemented
    val overriddenSymbols = mutable.Map.empty[TermSymbol, TermSymbol]

    val moduleDefinitions = module.members.sorted map { symbol =>
      val tpe = symbol.info.asSeenFrom(module, symbol.owner)
      (symbol, tpe.finalResultType.widen.asSeenFrom(module, module.typeSymbol), methodShapeType(tpe))
    }

    def relevantPeerDefinition(tpe: Type): Boolean =
      !(tpe <:< types.on) || tpe.typeArgs.size == 2 && peer <:< tpe.typeArgs(1)

    def abstractDefinition(symbol: Symbol): Boolean =
      symbol.isTerm &&
      relevantPeerDefinition(symbol.info.finalResultType.widen.asSeenFrom(module, module.typeSymbol)) &&
      (symbol.isAbstract ||
       (allAnnotations(symbol) exists { _.tree.tpe <:< types.abstractValue }) ||
       (symbol.asTerm.isStable && symbol.asTerm.isLazy && symbol.isFinal && {
         val sym = symbol.info.typeSymbol
         sym.isModuleClass &&
         (allAnnotations(sym) exists { _.tree.tpe <:< types.multitierModule }) &&
         (sym.info.members exists abstractDefinition)
       }))

    val abstractDefinitions = (module.members collect {
      case symbol if abstractDefinition(symbol) => symbol
    }).toSet

    placedValues.members.sorted foreach { symbol =>
      if (symbol.isTerm && (symbol.name != TermName("$loci$sys$create") || symbol.info.paramLists.nonEmpty)) {
        val baseSymbol = symbol.asTerm
        val baseInfo = baseSymbol.info.asSeenFrom(placedValues, baseSymbol.owner)
        val baseShapeType = methodShapeType(baseInfo)
        val baseName = baseSymbol.name
        val baseMultitierModule =
          baseSymbol.isStable &&
          baseSymbol.isLazy &&
          baseSymbol.isFinal &&
          baseInfo.finalResultType <:< types.placedValues && {
            val symbol = baseInfo.typeSymbol
            symbol.name == names.placedValues(symbol.owner) &&
            symbol.owner.isModuleClass &&
            (allAnnotations(symbol.owner) exists { _.tree.tpe <:< types.multitierModule })
          }

        def symbolOfType(symbol: TermSymbol) = {
          def tpe = symbol.info match {
            case MethodType(List(), MethodType(_, _)) => symbol.info
            case MethodType(List(), resultType) => resultType
            case NullaryMethodType(tpe) => tpe
            case _ => symbol.info
          }

          if (symbol.isModule)
            s"$symbol"
          else if (symbol.isSetter)
            s"$symbol of type ${symbol.asMethod.paramLists.head.head.info}"
          else
            s"$symbol of type $tpe"
        }

        val moduleDefinition = moduleDefinitions collectFirst {
          case definition @ (symbol, _, shapeType)
              if baseName == symbol.name && baseShapeType =:= shapeType=>
            definition
        }

        val peerDefinition = moduleDefinition exists { case (_, tpe, _) =>
          relevantPeerDefinition(tpe)
        }

        val abstractDefinition = moduleDefinition exists { case (symbol, _, _) =>
          abstractDefinitions contains symbol
        }

        val overridden = valueDefinitions exists { case (tree, symbol, name, shapeType) =>
          val getter = if (symbol.getter != NoSymbol) symbol.getter else symbol
          val overridden =
            if (!getter.isPrivateThis) {
              if (baseName == name && baseShapeType =:= shapeType) {
                if (!baseMultitierModule || !symbol.isModule) {
                  if (!peerDefinition)
                    c.abort(symbol.pos, s"overriding $symbol is not placed on $peer")

                  if (!(baseInfo.finalResultType =:= definitions.UnitTpe) &&
                      !(symbol.info.finalResultType <:< baseInfo.finalResultType))
                    c.abort(symbol.pos, s"overriding ${symbolOfType(symbol)} does not conform to base type")

                  if (!baseSymbol.isLazy && baseSymbol.isStable && (symbol.isLazy || symbol.isModule))
                    c.abort(symbol.pos, s"overriding $symbol must not be a lazy value")

                  if (baseSymbol.isLazy && !symbol.isLazy && !symbol.isModule)
                    c.abort(symbol.pos, s"overriding $symbol needs to be a lazy value")

                  if (baseSymbol.isStable && !symbol.isStable && !symbol.isLazy)
                    c.abort(symbol.pos, s"overriding $symbol needs to be stable")

                  if (baseSymbol.isFinal || baseSymbol.isModule)
                    c.abort(symbol.pos, s"$symbol cannot override final member")

                  if (baseSymbol.isVar)
                    c.abort(symbol.pos, s"$symbol cannot override variable")

                  if (!abstractDefinition && !(tree.mods hasFlag Flag.OVERRIDE))
                    c.abort(symbol.pos, s"$symbol needs `override` modifier")
                }
                true
              }
              else if (symbol.isVar &&
                  baseName == TermName(s"${name}_$$eq") &&
                  baseInfo.finalResultType =:= definitions.UnitTpe &&
                  baseInfo.paramLists.size == 1 &&
                  baseInfo.paramLists.head.size == 1 &&
                  baseInfo.paramLists.head.head.info =:= symbol.info.finalResultType)
                true
              else
                false
            }
            else
              false

          if (overridden)
            overriddenSymbols += symbol -> baseSymbol

          overridden
        }

        if (!overridden && abstractDefinition) {
          if (baseMultitierModule)
            c.abort(pos, s"object creation impossible since nested multitier module ${baseSymbol.name} is not defined")
          else
            c.abort(pos, s"object creation impossible since ${symbolOfType(baseSymbol)} is not defined")
        }
      }
    }

    // collect early and late definitions
    val lateSymbols = (valueDefinitions collect {
      case (_, symbol, _, _) if !symbol.isVar && !symbol.isVal || symbol.isLazy || symbol.isModule => symbol
    }).to[mutable.Set]

    var foundAdditionals = true
    while (foundAdditionals) {
      val additionals = (valueDefinitions collect {
        case (tree, symbol, _, _) if tree exists {
          case tree: RefTree if tree.symbol.isTerm =>
            val term = tree.symbol.asTerm
            val accessed = if (term.isAccessor) term.accessed else NoSymbol
            (lateSymbols contains term) ||
            accessed.isTerm && (lateSymbols contains accessed.asTerm)
          case _ =>
            false
        } =>
          symbol
      }).toSet -- lateSymbols

      if (additionals.isEmpty)
        foundAdditionals = false
      else
        lateSymbols ++= additionals
    }

    val (earlyDefinitions, lateDefinitions) =
      valueDefinitions.foldRight(List.empty[Tree] -> List.empty[Tree]) {
        case ((tree, symbol, _, _), (early, late)) =>
          val getter = if (symbol.getter != NoSymbol) symbol.getter.asTerm else symbol
          val flags = Flag.SYNTHETIC |
            (if (getter.isPrivateThis) Flag.PRIVATE | Flag.LOCAL else NoFlags) |
            (if (getter.isLazy) Flag.LAZY else NoFlags) |
            (if (getter.isImplicit) Flag.IMPLICIT else NoFlags) |
            (if (getter.isFinal) Flag.FINAL else NoFlags) |
            (if (symbol.isVar) Flag.MUTABLE else NoFlags)

          def returnTypeTree(symbol: TermSymbol, tpt: Tree) = {
            def returnsUnit(symbol: Symbol) =
              symbol.info.asSeenFrom(placedValues, symbol.owner).finalResultType =:= definitions.UnitTpe

            (overriddenSymbols get symbol
              collect { case symbol if returnsUnit(symbol) => TypeTree(definitions.UnitTpe) }
              getOrElse tpt)
          }

          val (overridingTrees, mods) = tree match {
            case tree @ ValDef(_, name, _, rhs) =>
              val mods = Modifiers(flags, typeNames.EMPTY, tree.mods.annotations)
              val tpt = returnTypeTree(symbol, tree.tpt)
              List(atPos(tree.pos) { ValDef(mods, name, tpt, rhs) }) -> tree.mods

            case tree @ DefDef(_, name, tparams, vparamss, _, rhs) =>
              val mods = Modifiers(flags, typeNames.EMPTY, tree.mods.annotations)
              val tpt = returnTypeTree(symbol, tree.tpt)
              List(atPos(tree.pos) { DefDef(mods, name, tparams, vparamss, tpt, rhs) }) -> tree.mods

            case tree @ ModuleDef(_, name, impl) =>
              val mods = Modifiers(flags, typeNames.EMPTY, tree.mods.annotations)

              if (allAnnotations(tree.symbol) exists { _.tree.tpe <:< types.multitierModule })
                c.abort(tree.pos, "multitier module overrides nothing")

              (overriddenSymbols get symbol
                map { symbol =>
                  val objectName = TermName(s"$$loci$$object$$$name")
                  val flags = Flag.SYNTHETIC | Flag.FINAL | Flag.LAZY |
                    (if (getter.isPrivateThis) Flag.PRIVATE | Flag.LOCAL else NoFlags)
                  List(
                    atPos(tree.pos) { ModuleDef(mods, objectName, impl) },
                    atPos(tree.pos) { q"$flags val $name: $objectName.type = $objectName" }) -> Modifiers()
                }
                getOrElse List(atPos(tree.pos) { ModuleDef(mods, name, impl) }) -> tree.mods)

            case _ =>
              List(tree) -> Modifiers()
          }

          if ((mods hasFlag Flag.OVERRIDE) && !(overriddenSymbols contains symbol))
            c.abort(tree.pos, s"$symbol module overrides nothing")

          if (lateSymbols contains symbol)
            early -> (overridingTrees ++ late)
          else
            (overridingTrees ++ early) -> late
      }

    earlyDefinitions -> lateDefinitions
  }
}
