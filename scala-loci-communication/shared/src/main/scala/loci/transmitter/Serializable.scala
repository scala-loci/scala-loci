package loci
package transmitter

import scala.util.Try
import scala.annotation.{compileTimeOnly, implicitNotFound}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

@implicitNotFound("${T} is not serializable")
trait Serializable[T] {
  def serialize(value: T): MessageBuffer
  def deserialize(value: MessageBuffer): Try[T]
}

object Serializable {
  @inline def apply[T](implicit serializable: Serializable[T]): Serializable[T] =
    serializable

  @compileTimeOnly("Value is not serializable")
  final implicit def resolutionFailure[T](implicit ev: DummyImplicit): Serializable[T] =
    macro SerializableResolutionFailure[T]

  @compileTimeOnly("Value is not serializable")
  final def dummy[T]: Serializable[T] = throw new NotImplementedError
}

object SerializableResolutionFailure {
  def apply[T: c.WeakTypeTag](c: whitebox.Context)(ev: c.Tree): c.Tree = {
    import c.universe._

    // the current macro expansion always appears twice
    // see: http://stackoverflow.com/a/20466423
    val recursionCount = c.openMacros.count { other =>
      c.enclosingPosition == other.enclosingPosition &&
        c.macroApplication.toString == other.macroApplication.toString
    }
    if (recursionCount > 2)
      c.abort(c.enclosingPosition, "Skipping serializable resolution failure macro for recursive invocation")

    val serializableType = weakTypeOf[Serializable[T]]

    if ((c inferImplicitValue serializableType).nonEmpty)
      c.abort(c.enclosingPosition, "Skipping serializable resolution failure macro to prioritize other implicit")

    val tpe = weakTypeOf[T]
    val message = s"$tpe is not serializable"

    q"""{
      @${termNames.ROOTPKG}.scala.annotation.compileTimeOnly($message) def resolutionFailure() = ()
      resolutionFailure()
      ${termNames.ROOTPKG}.loci.transmitter.Serializable.dummy[$tpe]
    }"""
  }
}
