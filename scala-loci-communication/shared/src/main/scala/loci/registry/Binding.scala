package loci
package registry

import transmitter.AbstractionRef
import transmitter.Marshallable
import transmitter.MarshallableArgument
import scala.util.Try
import scala.concurrent.Promise
import scala.concurrent.Future

trait Binding[T] {
  type RemoteCall
  val name: String
  def dispatch(function: T, message: MessageBuffer, abstraction: AbstractionRef)
    : Try[MessageBuffer]
  def call(abstraction: AbstractionRef)(handler: Binding.Handler)
    : RemoteCall
}

object Binding {
  type Handler = MessageBuffer => (MessageBuffer => Unit) => Unit

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
  protected def createCall[T](handler: Binding.Handler, message: MessageBuffer,
      marshallable: Marshallable[T], abstraction: AbstractionRef) = {
    val promise = Promise[marshallable.Result]
    handler(message) { message =>
      promise complete (marshallable unmarshal (message, abstraction))
    }
    promise.future
  }

  implicit def value[T](implicit res: Marshallable[T]) =
    new BindingBuilder.Value[T] {
      type RemoteCall = Future[res.Result]
      def apply(bindingName: String) = new Binding[T] {
        type RemoteCall = Future[res.Result]
        val name = bindingName
        def dispatch(
            value: T, message: MessageBuffer, abstraction: AbstractionRef) =
          Try { res marshal (value, abstraction) }
        def call(
            abstraction: AbstractionRef)(handler: Binding.Handler) =
          createCall(handler, MessageBuffer.empty, res, abstraction)
      }
    }
}

object BindingBuilder extends FunctionsBindingBuilder {
  trait Value[T] extends BindingBuilder[T]
}
