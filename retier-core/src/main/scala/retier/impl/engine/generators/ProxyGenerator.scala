package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait ProxyGenerator { this: Generation =>
  val c: Context
  import c.universe._

  val generateProxies = AugmentedAggregation[
    PeerDefinition with PlacedStatement, NonPlacedStatement] {
      aggregator =>

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

    def argumentTypesAsTupleTypeTree(argTypes: List[List[Type]]) =
      tq"""(..${argTypes map { types => tq"(..$types)" }})"""

    def argumentNamesAsTuple(argNames: List[List[TermName]]) =
      q"""(..${argNames map { names => q"(..$names)" }})"""

    def returnTypeAsTypeTree(returnType: Type) =
      typer createTypeTree returnType

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

    def implicitMarshallableLookup(typeTree: Tree) =
      q"""_root_.scala.Predef.implicitly[
            _root_.retier.transmission.Marshallable[$typeTree]]"""


    case class Proxy(peerType: Type, abstractionId: Tree,
      localResponseMarshallable: Tree, localResponse: CaseDef,
      remoteRequestMarshallable: Tree, remoteRequest: List[Tree])


    val decls = aggregator.all[PlacedStatement] collect {
      case stat @
          PlacedStatement(_: ValOrDefDef, _, _, Some(declTypeTree), _, _) =>
        stat
    }

    val proxies = decls.zipWithIndex map {
      case (PlacedStatement(decl: ValOrDefDef, peerType, exprType, Some(declTypeTree), _, _),
            index) =>
        val args = extractArguments(decl)
        val argTypes = extractArgumentTypes(args)
        val argNames = extractArgumentNames(args)

        val isNullary =
          argTypes.isEmpty || (argTypes.size == 1 && argTypes.head.isEmpty)
        val hasReturnValue =
          exprType =:!= typeOf[Unit] && exprType =:!= typeOf[Nothing]
        val isMutable = decl.mods hasFlag Flag.MUTABLE

        
        val abstractionId = generateAbstractionId(decl, decl.name, argTypes, exprType)
        val abstractionIdTermName = retierTermName(s"abstraction$$$index")
        val localResponseTermName = retierTermName(s"marshallable$$res$index")
        val remoteRequestTermName = retierTermName(s"marshallable$$req$index")

        val localResponseTypeTree = returnTypeAsTypeTree(exprType)

        val remoteRequestTypeTree =
          if (isMutable)
            localResponseTypeTree
          else
            argumentTypesAsTupleTypeTree(argTypes)


        val response =
          if (isMutable) {
            q"""if (request.isEmpty)
                  _root_.scala.util.Success {
                    $localResponseTermName marshall (${decl.name}, ref)
                  }
                else
                  $remoteRequestTermName unmarshall (request, ref) map { arg =>
                    ${decl.name} = arg; ""
                  }
             """
          }
          else {
            val methodCall =
              if (isNullary)
                q"""${decl.name}"""
              else
                q"""${decl.name}(...${applyTupleAsArguments(q"args", argTypes)})"""

            val methodCallResponding =
              if (hasReturnValue)
                q"""$localResponseTermName marshall ($methodCall, ref)"""
              else
                q"""$methodCall; """""

            if (isNullary)
              q"""_root_.scala.util.Success($methodCallResponding)"""
            else
              q"""$remoteRequestTermName unmarshall (request, ref) map { args =>
                    $methodCallResponding
                  }
               """
          }

        val request =
          if (isMutable) {
            val setter = TermName(s"${decl.name}_=")
            List(
              q"""def ${decl.name}: Unit =
                    ($abstractionIdTermName, "", $localResponseTermName)
               """,
              q"""def $setter(v: $localResponseTypeTree): Unit =
                    ($abstractionIdTermName, $remoteRequestTermName marshall (v, null))
               """)
          }
          else {
            val marshalled =
              if (isNullary)
                q""""""""
              else
                q"""$remoteRequestTermName marshall (${argumentNamesAsTuple(argNames)}, null)"""

            val marshalledReceiving =
              if (hasReturnValue)
                q"""($abstractionIdTermName, $marshalled, $localResponseTermName)"""
              else
                q"""($abstractionIdTermName, $marshalled)"""

            List(
              q"""def ${decl.name}(...$args): Unit =
                    $marshalledReceiving
               """)
           }


        Proxy(
          peerType,
          q"private[this] val $abstractionIdTermName = null: _root_.retier.transmission.AbstractionId",
          q"private[this] val $localResponseTermName = ${implicitMarshallableLookup(localResponseTypeTree)}",
          cq"`$abstractionIdTermName` => $response",
          q"private[this] val $remoteRequestTermName = ${implicitMarshallableLookup(remoteRequestTypeTree)}",
          request)
    }

    val nonPlacedStats = proxies flatMap { proxy =>
      Seq(
        NonPlacedStatement(proxy.abstractionId),
        NonPlacedStatement(proxy.localResponseMarshallable),
        NonPlacedStatement(proxy.remoteRequestMarshallable)
      )
    }

    val peerTypes = (aggregator.all[PeerDefinition] map { _.peer }).toSet

    val placedStats = proxies groupBy { _.peerType } flatMap {
      case (peerType, proxies) =>
        val cases = proxies map { _.localResponse }
        val respond =
          q"""def $$$$retier$$respond(
                  request: String,
                  id: _root_.retier.transmission.AbstractionId,
                  ref: _root_.retier.transmission.AbstractionRef)
                  : _root_.scala.util.Try[_root_.scala.Predef.String] =
                id match { case ..$cases }
           """

        val response = {
          val DefDef(_, _, _, _, _, rhs) = respond
          PlacedStatement(respond, peerType, NoType, Some(TypeTree()), None, rhs)
        }

        val requests =
          (proxies flatMap {
            _.remoteRequest flatMap { request =>
              val DefDef(_, _, _, _, _, rhs) = request
              (peerTypes - peerType) map { peerType =>
                PlacedStatement(request, peerType, NoType, Some(TypeTree()), None, rhs)
              }
            }
          })

        response +: requests
    }

    aggregator add nonPlacedStats add placedStats
  }
}
