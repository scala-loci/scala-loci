package loci
package transmitter
package transmittable

import scala.annotation.compileTimeOnly
import scala.concurrent.Future
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait TransmittableDummy {
  this: TransmittableBase.type =>

  @compileTimeOnly("Value is not transmittable")
  final implicit def resolutionFailure[T]: Transmittable.Any[T, T, T] {
    type Proxy = Future[T]
    type Transmittables = Transmittables.None
  } = macro TransmittableResolutionFailure[T]

  @compileTimeOnly("Value is not transmittable")
  final def dummy[T]: Transmittable.Any[T, T, T] {
    type Proxy = Future[T]
    type Transmittables = Transmittables.None
  } = IdenticallyTransmittable()
}

object TransmittableResolutionFailure {
  def apply[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val (tpe, original) = weakTypeOf[T] match {
      case tpe @ TypeRef(_, _, List(_, _, ConstantType(Constant(original: String))))
          if tpe <:< typeOf[TransmittableBase.SurrogateType[_, _, _]] =>
        tpe -> original
      case tpe =>
        tpe -> tpe.toString
    }

    val message = s"$original is not transmittable"

    q"""{
      @${termNames.ROOTPKG}.scala.annotation.compileTimeOnly($message) def resolutionFailure() = ()
      resolutionFailure()
      ${termNames.ROOTPKG}.loci.transmitter.transmittable.TransmittableBase.dummy[$tpe]
    }"""
  }
}
