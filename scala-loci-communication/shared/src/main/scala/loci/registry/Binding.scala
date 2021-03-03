package loci
package registry

import transmitter._

import scala.util.Try

trait Binding[T, R] {
  val name: String

  def dispatch(
    function: RemoteRef => T,
    message: MessageBuffer,
    abstraction: AbstractionRef): Try[MessageBuffer]

  def call[A <: AbstractionRef](
    createAbstraction: () => A)(
    handler: (MessageBuffer, A) => Notice.Steady[Try[MessageBuffer]]): R
}

object Binding {
  def apply[T](name: String)(implicit builder: BindingBuilder[T]) =
    builder(name)

  def Value[T](name: String)(implicit builder: BindingBuilder.Value[T]) =
    builder(name)
}

trait BindingBuilder[T] {
  type Result
  def apply(bindingName: String): Binding[T, Result]
}

trait ValueBindingBuilder {
  implicit def value[T, P](implicit res: Marshallable[T, _, P])
      : BindingBuilder.Value[T] { type Result = P } =
    new BindingBuilder.Value[T] {
      type Result = P

      def apply(bindingName: String) = new Binding[T, Result] {
        val name = bindingName

        def dispatch(
            function: RemoteRef => T,
            message: MessageBuffer,
            abstraction: AbstractionRef) =
          Try { res.marshal(function(abstraction.remote), abstraction) }

        def call[A <: AbstractionRef](
            createAbstraction: () => A)(
            handler: (MessageBuffer, A) => Notice.Steady[Try[MessageBuffer]]) = {
          val abstraction = createAbstraction()
          res.unmarshal(handler(MessageBuffer.empty, abstraction), abstraction)
        }
      }
    }
}

object BindingBuilder extends FunctionsBindingBuilder {
  trait Value[T] extends BindingBuilder[T]
}


trait SubjectiveBinding[T, U] {
  def apply(remote: RemoteRef, function: T): U
}

trait ValueSubjectiveBinding {
  implicit def value[R]
      : SubjectiveBinding[RemoteRef => R, R] =
    new SubjectiveBinding[RemoteRef => R, R] {
      def apply(remote: RemoteRef, function: RemoteRef => R) = function(remote)
    }
}

object SubjectiveBinding extends FunctionSubjectiveBinding
