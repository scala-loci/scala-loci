package loci
package embedding
package impl

import utility.reflectionExtensions.*

import scala.quoted.*

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
          Lambda(symbol, tpe, { (symbol, args) =>
            (lambdaArgs zip args).foldLeft(rhs.changeOwner(symbol)) { case (rhs, (lambdaArg, arg)) =>
              changeRefs(lambdaArg.symbol, arg.symbol, symbol, rhs)
            }
          })

    case _ =>
      block
end swapLambdaResult