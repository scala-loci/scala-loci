package loci
package serializer

import utility.reflectionExtensions.*

import scala.annotation.{compileTimeOnly, implicitNotFound}
import scala.util.Try
import scala.quoted.*

@implicitNotFound("${T} is not serializable")
trait Serializable[T]:
  def serialize(value: T): MessageBuffer
  def deserialize(value: MessageBuffer): Try[T]

object Serializable:
  def apply[T](using serializable: Serializable[T]) = serializable

  @compileTimeOnly("Value is not serializable")
  transparent inline given resolutionFailure[T, SerializableFallback[_]](using
      inline ev: Serializable[T] =:= SerializableFallback[T])
    : SerializableFallback[T] =
      ${ resolutionFailureImpl[T, SerializableFallback[T]] }

  @compileTimeOnly("Value is not serializable")
  def dummy[T]: Serializable[T] = throw new NotImplementedError

  def resolutionFailureImpl[T: Type, SerializableFallback: Type](using Quotes) =
    import quotes.reflect.*

    val baseMessage = s"${TypeRepr.of[T].safeShow("Value")} is not serializable"
    val message = s"$baseMessage${utility.implicitHints.values(TypeRepr.of[Serializable[T]])}"

    val Inlined(_, _, Block(List(resolutionFailure), _)) = '{
      @compileTimeOnly(${Expr(message)}) def resolutionFailure() = ()
    }.asTerm: @unchecked

    Block(
        List(resolutionFailure, Ref(resolutionFailure.symbol).appliedToNone),
        '{ dummy[T] }.asTerm)
      .asExprOf[SerializableFallback]
  end resolutionFailureImpl
end Serializable
