package loci
package transmitter

import scala.concurrent.duration.{ Duration, FiniteDuration }

trait TransmittablePrimitives {
  this: Transmittable.type =>

  @inline final implicit def nothing: IdenticallyTransmittable[Nothing] =
    IdenticallyTransmittable()
  @inline final implicit def unit: IdenticallyTransmittable[Unit] =
    IdenticallyTransmittable()
  @inline final implicit def boolean: IdenticallyTransmittable[Boolean] =
    IdenticallyTransmittable()
  @inline final implicit def char: IdenticallyTransmittable[Char] =
    IdenticallyTransmittable()
  @inline final implicit def byte: IdenticallyTransmittable[Byte] =
    IdenticallyTransmittable()
  @inline final implicit def short: IdenticallyTransmittable[Short] =
    IdenticallyTransmittable()
  @inline final implicit def int: IdenticallyTransmittable[Int] =
    IdenticallyTransmittable()
  @inline final implicit def long: IdenticallyTransmittable[Long] =
    IdenticallyTransmittable()
  @inline final implicit def float: IdenticallyTransmittable[Float] =
    IdenticallyTransmittable()
  @inline final implicit def double: IdenticallyTransmittable[Double] =
    IdenticallyTransmittable()
  @inline final implicit def string: IdenticallyTransmittable[String] =
    IdenticallyTransmittable()
  @inline final implicit def symbol: IdenticallyTransmittable[Symbol] =
    IdenticallyTransmittable()
  @inline final implicit def bigInt: IdenticallyTransmittable[BigInt] =
    IdenticallyTransmittable()
  @inline final implicit def bigDecimal: IdenticallyTransmittable[BigDecimal] =
    IdenticallyTransmittable()
  @inline final implicit def duration: IdenticallyTransmittable[Duration] =
    IdenticallyTransmittable()
  @inline final implicit def finiteDuration: IdenticallyTransmittable[FiniteDuration] =
    IdenticallyTransmittable()
  @inline final implicit def infiniteDuration: IdenticallyTransmittable[Duration.Infinite] =
    IdenticallyTransmittable()

  @inline final implicit def javaBoolean: IdenticallyTransmittable[java.lang.Boolean] =
    IdenticallyTransmittable()
  @inline final implicit def javaChar: IdenticallyTransmittable[java.lang.Character] =
    IdenticallyTransmittable()
  @inline final implicit def javaByte: IdenticallyTransmittable[java.lang.Byte] =
    IdenticallyTransmittable()
  @inline final implicit def javaShort: IdenticallyTransmittable[java.lang.Short] =
    IdenticallyTransmittable()
  @inline final implicit def javaInt: IdenticallyTransmittable[java.lang.Integer] =
    IdenticallyTransmittable()
  @inline final implicit def javaLong: IdenticallyTransmittable[java.lang.Long] =
    IdenticallyTransmittable()
  @inline final implicit def javaFloat: IdenticallyTransmittable[java.lang.Float] =
    IdenticallyTransmittable()
  @inline final implicit def javaDouble: IdenticallyTransmittable[java.lang.Double] =
    IdenticallyTransmittable()
  @inline final implicit def javaBigInteger: IdenticallyTransmittable[java.math.BigInteger] =
    IdenticallyTransmittable()
  @inline final implicit def javaBigDecimal: IdenticallyTransmittable[java.math.BigDecimal] =
    IdenticallyTransmittable()
  @inline final implicit def javaUuid: IdenticallyTransmittable[java.util.UUID] =
    IdenticallyTransmittable()
  @inline final implicit def javaDate: IdenticallyTransmittable[java.util.Date] =
    IdenticallyTransmittable()
}
