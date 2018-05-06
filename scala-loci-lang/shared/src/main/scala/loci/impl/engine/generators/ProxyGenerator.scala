package loci
package impl
package engine.generators

import engine._
import scala.reflect.NameTransformer
import scala.reflect.macros.blackbox.Context

trait ProxyGenerator { this: Generation =>
  val c: Context
  import c.universe._

  val generateProxies = AugmentedAggregation[
    PlacedStatement, PlacedAbstraction] {
      aggregator =>

    echo(verbose = true, " Generating shared abstraction proxies")

    val synthetic = Flag.SYNTHETIC
    val artifact = Flag.ARTIFACT

    def generateAbstractionId(tree: Tree, name: TermName,
        argTypes: List[List[Type]], resultType: Type) = {
      if ((resultType exists { _.isGeneric }) ||
          (argTypes exists { _ exists { _.isGeneric } }))
        c.abort(tree.pos,
          "placed methods cannot be parameterized over " +
          "their argument types or their return type")

      name.encodedName.toString +
      (argTypes map {
        _ map { _.typeSymbol.fullName } mkString ("(", ",", ")")
      }).mkString
    }

    def extractArgumentTypes(args: List[List[ValDef]]) =
      args map { _ map { _.tpt.tpe } }

    def extractArgumentNames(args: List[List[ValDef]]) =
      args map { _ map { _.name } }

    def extractArgumentPositions(args: List[List[ValDef]]) =
      args map { _ map { _.pos } }

    def extractArguments(tree: ValOrDefDef) =
      tree match {
        case DefDef(_, _, _, vparamss, _, _) => vparamss
        case ValDef(_, _, _, _) => List.empty
      }

    def argumentTypesAsTupleTypeTree(argTypes: List[List[(Type, Position)]]) = {
      val types = argTypes map { typesPositions =>
        val types = typesPositions map { case (tpe, pos) =>
          typer createTypeTree (tpe, pos)
        }
        tq"(..$types)"
      }
      tq"(..$types)"
    }

    def argumentNamesAsTuple(argNames: List[List[TermName]]) = {
      val names = argNames map { names => q"(..$names)" }
      q"(..$names)"
    }

    def applyTupleAsArguments(tuple: Tree, args: List[List[_]]) = {
      def tupleAccessor(index: Int) = TermName(s"_$index")

      def applyTupleAsArguments(tuple: Tree, args: List[_]) =
        if (args.size != 1)
          args.zipWithIndex map { case (_, index) =>
            q"$tuple.${tupleAccessor(index + 1)}"
          }
        else
          List(tuple)

      if (args.size != 1)
        args.zipWithIndex map { case (arg, index) =>
          applyTupleAsArguments(q"$tuple.${tupleAccessor(index + 1)}", arg)
        }
      else
        List(applyTupleAsArguments(tuple, args.head))
    }

    val decls = aggregator.all[PlacedStatement] collect {
      case stat @ PlacedStatement(
            decl: ValOrDefDef, _, _, Some(declTypeTree), _, _, _)
          if decl.tpt.tpe <:< types.sharedOn =>
        stat
    }

    val peerDecls = decls groupBy { _.peerSymbol.name }

    val abstractions = peerDecls flatMap { case (peerName, stats) =>
      stats.zipWithIndex map {
        case (PlacedStatement(decl: ValOrDefDef, peerSymbol, exprType,
                              Some(declTypeTree), _, _, _),
              index) =>
        import trees._

        val args = extractArguments(decl)
        val argTypes = extractArgumentTypes(args)
        val argNames = extractArgumentNames(args)
        val argPositions = extractArgumentPositions(args)

        val isBottomType = types.bottom exists { exprType <:< _ }
        val isSubjective = !isBottomType &&
          (types.subjectivePlacing exists { exprType <:< _ })
        val isControlledSubjective = !isBottomType &&
          (types.controlledSubjectivePlacing exists { exprType <:< _ })

        val (valueType, valueTypeTree) =
          if (isSubjective || isControlledSubjective)
            (exprType.typeArgs.last, declTypeTree.typeArgTrees.last)
          else
            (exprType, declTypeTree)

        val isMutable = decl.mods hasFlag Flag.MUTABLE
        val isStable = decl match {
          case ValDef(_, _, _, _) => !isMutable
          case DefDef(_, _, _, _, _, _) => false
        }
        val isNullary = (argTypes.headOption flatMap { _.headOption }).isEmpty
        val hasReturnValue = valueType =:!= definitions.UnitTpe

        val abstractionId = generateAbstractionId(
          decl, decl.name, argTypes, valueType)
        val abstractionIdTermName = lociTermName(s"abs$$$index")
        val localResponseTermName = lociTermName(s"mar$$$index$$res")
        val remoteRequestTermName = lociTermName(s"mar$$$index$$req")
        val declTermName = decl.name

        val abstractionIdTerm = q"${names.interface}.$abstractionIdTermName"
        val localResponseTerm = q"${names.interface}.$localResponseTermName"
        val remoteRequestTerm = q"${names.interface}.$remoteRequestTermName"
        val declTerm = q"${names.implementation}.this.$declTermName"

        val localResponseTypeTree = valueTypeTree

        val remoteRequestTypeTree =
          if (isMutable)
            localResponseTypeTree
          else
            argumentTypesAsTupleTypeTree(
              argTypes zip argPositions map {
                case (types, positions) => types zip positions
              })

        val response = {
          val arguments = applyTupleAsArguments(q"args", argTypes)
          val declInvocation =
            if (isControlledSubjective)
              q"""$TryCreate { $declTerm(...$arguments)(remote) }"""
            else
              q"""$TryCreate { $declTerm(...$arguments) }"""

          val response =
            if (isMutable) {
              q"""if (request.isEmpty)
                    $declInvocation map { response =>
                      $localResponseTerm marshal (response, ref)
                    }
                  else
                    $remoteRequestTerm unmarshal (request, ref) map { arg =>
                      $declTerm = arg; $MessageBufferEmpty
                    }
               """
            }
            else {
              val marshalled =
                if (hasReturnValue)
                  q"""$declInvocation map { response =>
                        $localResponseTerm marshal (response, ref)
                      }
                   """
                else
                  q"""$declInvocation map { response => $MessageBufferEmpty }"""

              if (isNullary)
                q"""$marshalled"""
              else
                q"""$remoteRequestTerm unmarshal (request, ref) flatMap { args =>
                      $marshalled
                    }
                 """
            }

          if (isControlledSubjective) {
            val remoteTypeTree = decl.tpt.typeArgTrees.head.typeArgTrees.head
            val peerTypeTree = remoteTypeTree.typeArgTrees.head

            q"""$TryCreate {
                  new $RemoteRefOps(
                    new $AbstractionRefOps(ref).remote
                  ).asRemote[$peerTypeTree].get
                } flatMap { remote =>
                  $response
                }
             """
          }
          else
            response
        }

        val request = {
          def transmissionPropertiesCreate(marshallable: Tree, request: Tree,
              requestMarshallable: Tree) =
            q"""$TransmissionPropertiesCreate(
                  $abstractionIdTerm, $marshallable,
                  $request, $requestMarshallable)"""

          if (isMutable) {
            val declSetterTermName =
              TermName(NameTransformer encode s"${declTermName}_=")

            val getterTransmissionProperties = transmissionPropertiesCreate(
              localResponseTerm, q"()", UnitMarshallable)

            val setterTransmissionProperties = transmissionPropertiesCreate(
              UnitMarshallable, q"v", remoteRequestTerm)

            Seq(
              q"""def $declTermName = $getterTransmissionProperties""",
              q"""def ${declSetterTermName}(v: $localResponseTypeTree) =
                    $setterTransmissionProperties""")
          }
          else {
            val (request, requestMarshallable) =
              if (isNullary)
                (q"()", UnitMarshallable)
              else
                (q"${argumentNamesAsTuple(argNames)}", remoteRequestTerm)

            val marshallable =
              if (hasReturnValue)
                q"$localResponseTerm"
              else
                q"$UnitMarshallable"

            val transmissionProperties = transmissionPropertiesCreate(
              marshallable, request, requestMarshallable)

            val flags =
              if (decl.isLociSynthetic) synthetic | artifact
              else NoFlags

            Seq(q"$flags def $declTermName(...$args) = $transmissionProperties")
          }
        }

        val abstractionIdDef =
          q"""$synthetic private[$peerName] val $abstractionIdTermName =
                $AbstractionIdCreate($abstractionId, $isStable)"""

        val localResponseDef =
          if (isMutable || hasReturnValue)
            Some(markLociSynthetic(
              q"""$synthetic private[$peerName] val $localResponseTermName =
                    $Marshallable[$localResponseTypeTree]""",
              decl.pos))
          else
            None

        val remoteRequestDef =
          if (isMutable || !isNullary)
            Some(markLociSynthetic(
              q"""$synthetic private[$peerName] val $remoteRequestTermName =
                    $MarshallableArgument[$remoteRequestTypeTree]""",
              decl.pos))
          else
            None

        val dispatchClause = markLociSynthetic(
          cq"$abstractionIdTerm => $response", decl.pos)

        PlacedAbstraction(
          peerSymbol,
          Seq(abstractionIdDef).toList ++
            localResponseDef.toList ++ remoteRequestDef.toList ++ request,
          dispatchClause)
      }
    }

    echo(verbose = true, s"  [${abstractions.size} placed abstractions added]")

    aggregator add abstractions
  }
}
