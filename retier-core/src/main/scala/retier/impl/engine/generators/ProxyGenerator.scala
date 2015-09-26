package retier
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

    def generateAbstractionId(tree: Tree, name: TermName,
        argTypes: List[List[Type]], resultType: Type) = {
      if (resultType.isGeneric ||
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

    def extractArguments(tree: ValOrDefDef) =
      tree match {
        case DefDef(_, _, _, vparamss, _, _) => vparamss
        case ValDef(_, _, _, _) => List.empty
      }

    def typeAsTypeTree(tpe: Type) = typer createTypeTree tpe

    def argumentTypesAsTupleTypeTree(argTypes: List[List[Type]]) = {
      val types = argTypes map { types => tq"(..${types map typeAsTypeTree })" }
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
      case stat @
            PlacedStatement(decl: ValOrDefDef, _, _, Some(declTypeTree), _, _)
          if decl.tpt.tpe <:< types.sharedOn =>
        stat
    }

    val peerDecls = decls groupBy { _.peerSymbol.name }

    val abstractions = peerDecls flatMap { case (peerName, stats) =>
      stats.zipWithIndex map {
        case (PlacedStatement(decl: ValOrDefDef, peerSymbol, exprType,
                              Some(declTypeTree), _, _),
              index) =>
        import trees._

        val args = extractArguments(decl)
        val argTypes = extractArgumentTypes(args)
        val argNames = extractArgumentNames(args)

        val isMutable = decl.mods hasFlag Flag.MUTABLE
        val isStable = decl match {
          case ValDef(_, _, _, _) => !isMutable
          case DefDef(_, _, _, _, _, _) => false
        }
        val isNullary = (argTypes.headOption flatMap { _.headOption }).isEmpty
        val hasReturnValue = exprType =:!= definitions.UnitTpe

        val abstractionId = generateAbstractionId(decl, decl.name, argTypes, exprType)
        val abstractionIdTermName = retierTermName(s"abs$$$index")
        val localResponseTermName = retierTermName(s"mar$$$index$$res")
        val remoteRequestTermName = retierTermName(s"mar$$$index$$req")
        val declTermName = decl.name

        val abstractionIdTerm = q"${names.interface}.$abstractionIdTermName"
        val localResponseTerm = q"${names.interface}.$localResponseTermName"
        val remoteRequestTerm = q"${names.interface}.$remoteRequestTermName"
        val declTerm = q"${names.implementation}.this.$declTermName"

        val localResponseTypeTree = typeAsTypeTree(exprType)

        val remoteRequestTypeTree =
          if (isMutable)
            localResponseTypeTree
          else
            argumentTypesAsTupleTypeTree(argTypes)

        val response = {
          if (isMutable) {
            q"""if (request.isEmpty)
                  $Success($localResponseTerm marshall ($declTerm, ref))
                else
                  $remoteRequestTerm unmarshall (request, ref) map { arg =>
                    $declTerm = arg; ""
                  }
             """
          }
          else {
            val arguments = applyTupleAsArguments(q"args", argTypes)

            val response =
              if (isNullary)
                q"""$declTerm"""
              else
                q"""$declTerm(...$arguments)"""

            val marshalled =
              if (hasReturnValue)
                q"""$localResponseTerm marshall ($response, ref)"""
              else
                q"""$response; """""

            if (isNullary)
              q"""$Success($marshalled)"""
            else
              q"""$remoteRequestTerm unmarshall (request, ref) map { args =>
                    $marshalled
                  }
               """
          }
        }

        val request = {
          def transmissionPropertiesCreate(marshallable: Tree, request: Tree) =
            q"""$TransmissionPropertiesCreate(
                  $abstractionIdTerm, $marshallable, $request)"""

          if (isMutable) {
            val declSetterTermName =
              TermName(NameTransformer encode s"${declTermName}_=")

            val getterTransmissionProperties = transmissionPropertiesCreate(
              q"$localResponseTerm", q"($UnitMarshallable, ())")

            val setterTransmissionProperties = transmissionPropertiesCreate(
              q"$UnitMarshallable", q"($remoteRequestTerm, v)")

            Seq(
              q"""def $declTermName = $getterTransmissionProperties""",
              q"""def ${declSetterTermName}(v: $localResponseTypeTree) =
                    $setterTransmissionProperties""")
          }
          else {
            val request =
              if (isNullary)
                q"($UnitMarshallable, ())"
              else
                q"($remoteRequestTerm, ${argumentNamesAsTuple(argNames)})"

            val marshallable =
              if (hasReturnValue)
                q"$localResponseTerm"
              else
                q"$UnitMarshallable"

            val transmissionProperties =
              transmissionPropertiesCreate(marshallable, request)

            Seq(q"def $declTermName(...$args) = $transmissionProperties")
          }
        }

        val abstractionIdDef =
          q"""$synthetic private[$peerName] val $abstractionIdTermName =
                $AbstractionIdCreate($abstractionId, $isStable)"""

        val localResponseDef =
          if (isMutable || hasReturnValue)
            Some(markRetierSynthetic(
              q"""$synthetic private[$peerName] val $localResponseTermName =
                    $implicitly[$Marshallable[$localResponseTypeTree]]""",
              decl.pos))
          else
            None

        val remoteRequestDef =
          if (isMutable || !isNullary)
            Some(markRetierSynthetic(
              q"""$synthetic private[$peerName] val $remoteRequestTermName =
                    $implicitly[$Marshallable[$remoteRequestTypeTree]]""",
              decl.pos))
          else
            None

        val dispatchClause = markRetierSynthetic(
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
