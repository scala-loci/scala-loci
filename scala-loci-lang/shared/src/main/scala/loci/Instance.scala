package loci

import loci.language._

import scala.annotation.compileTimeOnly
import scala.concurrent.ExecutionContext
import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}

class Instance[P] private[loci] (dummy: Int) {
  @compileTimeOnly("Multitier peer instantiation must be of the form: multitier start new Instance[P]")
  def this() = this(0)

  @compileTimeOnly("Multitier peer instantiation must be of the form: multitier start new Instance[P]")
  def this(connect: Connections) = this(0)

  @compileTimeOnly("Multitier peer instantiation must be of the form: multitier start new Instance[P]")
  def this(context: ExecutionContext) = this(0)

  @compileTimeOnly("Multitier peer instantiation must be of the form: multitier start new Instance[P]")
  def this(separateMainThread: Boolean) = this(0)

  @compileTimeOnly("Multitier peer instantiation must be of the form: multitier start new Instance[P]")
  def this(context: ExecutionContext, connect: Connections) = this(0)

  @compileTimeOnly("Multitier peer instantiation must be of the form: multitier start new Instance[P]")
  def this(separateMainThread: Boolean, connect: Connections) = this(0)

  @compileTimeOnly("Multitier peer instantiation must be of the form: multitier start new Instance[P]")
  def this(context: ExecutionContext, separateMainThread: Boolean) = this(0)

  @compileTimeOnly("Multitier peer instantiation must be of the form: multitier start new Instance[P]")
  def this(context: ExecutionContext, separateMainThread: Boolean, connect: Connections) = this(0)
}

object Instance {
  final implicit class Ops[P](instance: Instance[P]) {
    def retrieve[T](retrievable: Retrievable[T]): T = macro language.impl.Instance.retrieve
    def terminate(): Unit = instance match {
      case instance: runtime.Instance[P] => instance.terminate()
      case _ => throw new runtime.PeerImplementationError
    }
    def terminated: Notice.Steady[Unit] = instance match {
      case instance: runtime.Instance[P] => instance.terminated
      case _ => throw new runtime.PeerImplementationError
    }
  }


  trait SubjectiveValue[T, R] {
    def to(remote: Remote[R]): T
  }


  sealed trait Retrievable[T]

  sealed trait RetrievableDefault {
    implicit def default[T](v: T): Retrievable[T] = erased
  }

  sealed trait RetrievablePlaced extends RetrievableDefault {
    implicit def placed[T, P](v: T on P): Retrievable[T] = erased
  }

  sealed trait RetrievableLocal extends RetrievablePlaced {
    implicit def local[T, P, _Local_[T] <: Local[T]](v: _Local_[T] on P): Retrievable[T] = erased
  }

  object Retrievable extends RetrievableLocal {
    implicit def subjective[T, R, P](v: T per R on P): Retrievable[SubjectiveValue[T, P]] = erased
  }
}
