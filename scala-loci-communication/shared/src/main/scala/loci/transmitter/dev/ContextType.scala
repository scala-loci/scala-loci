package loci
package transmitter
package dev

import scala.annotation.implicitNotFound

@implicitNotFound("Required context not found: ${U}")
sealed trait ContextType[-T <: Context[_, _], +U <: Context[_, _]]
  extends (T => U)

object ContextType {
  @inline implicit def contextType[T <: Context[_, _], U <: Context[_, _]](
      implicit ev: T <:< U) = {
    unused(ev)
    singleton.asInstanceOf[ContextType[T, U]]
  }

  @inline private[this] def unused(v: Any) = v

  private[this] val singleton = new ContextType[
      Context[NoTransmittables, NoMessage],
      Context[NoTransmittables, NoMessage]] {
    def apply(v: Context[NoTransmittables, NoMessage]) = v
  }
}
