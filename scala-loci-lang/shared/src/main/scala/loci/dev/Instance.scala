package loci.dev

import loci.dev.language.Connections

import scala.annotation.compileTimeOnly
import scala.concurrent.ExecutionContext

class Instance[P] private[dev] (dummy: Int) {
  @compileTimeOnly("Multitier peer instantiation must be of the form: multitier start new Instance[P]")
  def this() = this(0)

  @compileTimeOnly("Multitier peer instantiation must be of the form: multitier start new Instance[P]")
  def this(connect: Connections) = this(0)

  @compileTimeOnly("Multitier peer instantiation must be of the form: multitier start new Instance[P]")
  def this(context: ExecutionContext) = this(0)

  @compileTimeOnly("Multitier peer instantiation must be of the form: multitier start new Instance[P]")
  def this(context: ExecutionContext, connect: Connections) = this(0)
}

object Instance {
  final implicit class Ops[P](instance: Instance[P]) {
    def terminate(): Unit = instance match {
      case instance: runtime.Instance[P] => instance.terminate()
      case _ => throw new IllegalAccessException("Illegal access to peer instance")
    }
  }
}
