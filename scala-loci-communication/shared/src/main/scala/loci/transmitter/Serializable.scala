package loci
package transmitter

import scala.annotation.{compileTimeOnly, implicitNotFound}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.util.Try

@implicitNotFound("${T} is not serializable")
trait Serializable[T] {
  def serialize(value: T): MessageBuffer
  def deserialize(value: MessageBuffer): Try[T]
}

object Serializable {
  @inline def apply[T](implicit serializable: Serializable[T]): Serializable[T] =
    serializable

  @compileTimeOnly("Value is not serializable")
  final implicit def resolutionFailure[T, S[T]]: S[T] =
    macro SerializableResolutionFailure[T]

  @compileTimeOnly("Value is not serializable")
  final def dummy[T]: Serializable[T] = throw new NotImplementedError
}

object SerializableResolutionFailure {
  def apply[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val message = s"$tpe is not serializable"

    q"""{
      @${termNames.ROOTPKG}.scala.annotation.compileTimeOnly($message) def resolutionFailure() = ()
      resolutionFailure()
      ${termNames.ROOTPKG}.loci.transmitter.Serializable.dummy[$tpe]
    }"""
  }
}
