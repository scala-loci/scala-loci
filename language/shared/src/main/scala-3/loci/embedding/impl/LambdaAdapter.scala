package loci
package embedding
package impl

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.quoted.*

@experimental
def swapLambdaResult(using Quotes)(block: quotes.reflect.Block, resultType: quotes.reflect.TypeRepr, rhs: quotes.reflect.Term) =
  import quotes.reflect.*

  block match
    case Lambda(lambdaArgs, _) =>
      val Block(List(lambda: DefDef), closure) = block: @unchecked
      val symbol = lambda.symbol
      val MethodType(paramNames, paramTypes, _) = symbol.info: @unchecked
      val tpe = MethodType(paramNames)(_ => paramTypes, _ => resultType)

      SymbolMutator.get match
        case Some(symbolMutator) =>
          symbolMutator.setInfo(symbol, tpe)
          Block.copy(block)(List(DefDef.copy(lambda)(lambda.name, lambda.paramss, TypeTree.of(using resultType.asType), Some(rhs))), closure)
        case _ =>
          Lambda(symbol, tpe, (symbol, args) =>
            rhs.changeOwner(symbol).substituteRefs(((lambdaArgs map { _.symbol }) zip (args map { _.symbol })).toMap, symbol))

    case _ =>
      block
end swapLambdaResult
