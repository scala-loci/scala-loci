package loci
package registry

import transmitter.AbstractionRef
import transmitter.Marshallable
import transmitter.RemoteRef
import scala.util.Try
import scala.concurrent.Future

trait Binding[T] {
  type RemoteCall
  val name: String
  def dispatch(function: RemoteRef => T, message: MessageBuffer, abstraction: AbstractionRef)
    : Try[MessageBuffer]
  def call(abstraction: AbstractionRef)(handler: MessageBuffer => Future[MessageBuffer])
    : RemoteCall
}

object Binding {
  def apply[T](name: String)(implicit builder: BindingBuilder[T]) =
    builder(name)

  def value[T](name: String)(implicit builder: BindingBuilder.Value[T]) =
    builder(name)
}

trait BindingBuilder[T] {
  type RemoteCall
  def apply(bindingName: String)
    : Binding[T] { type RemoteCall = BindingBuilder.this.RemoteCall }
}

trait ValueBindingBuilder {
  implicit def value[T, P](implicit res: Marshallable[T, _, P]) =
    new BindingBuilder.Value[T] {
      type RemoteCall = P

      def apply(bindingName: String) = new Binding[T] {
        type RemoteCall = P
        val name = bindingName

        def dispatch(
            function: RemoteRef => T,
            message: MessageBuffer,
            abstraction: AbstractionRef) =
          Try { res.marshal(function(abstraction.remote), abstraction) }

        def call(
            abstraction: AbstractionRef)(
            handler: MessageBuffer => Future[MessageBuffer]) =
          res.unmarshal(handler(MessageBuffer.empty), abstraction)
      }
    }
}

object BindingBuilder extends FunctionsBindingBuilder {
  trait Value[T] extends BindingBuilder[T]
}
