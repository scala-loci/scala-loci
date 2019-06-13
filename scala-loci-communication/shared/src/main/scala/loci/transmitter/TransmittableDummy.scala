package loci
package transmitter

import scala.annotation.compileTimeOnly
import scala.concurrent.Future
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

trait TransmittableDummy {
  this: Transmittable.type =>

  @compileTimeOnly("Value is not transmittable")
  final implicit def resolutionFailure[T]: Transmittable[T, T, T] {
    type Proxy = Future[T]
    type Transmittables = Transmittables.None
  } = macro TransmittableResolutionFailure[T]

  @compileTimeOnly("Value is not transmittable")
  final def dummy[T]: Transmittable[T, T, T] {
    type Proxy = Future[T]
    type Transmittables = Transmittables.None
  } = IdenticallyTransmittable()
}

object TransmittableResolutionFailure {
  def apply[T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val message = s"$tpe is not transmittable"

    q"""{
      @${termNames.ROOTPKG}.scala.annotation.compileTimeOnly($message) def resolutionFailure() = ()
      resolutionFailure()
      ${termNames.ROOTPKG}.loci.transmitter.Transmittable.dummy[$tpe]
    }"""
  }
}
