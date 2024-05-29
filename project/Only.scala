import sbt._
import sbt.Keys._

import scala.collection.generic.CanBuildFrom
import scala.collection.TraversableLike
import scala.language.higherKinds
import scala.util.matching.Regex

object only extends Selection("".r, Function.const(true)) {
  def jvm = new Selection(raw"[/\\]\.?jvm[/\\]".r, Function.const(true))
  def js = new Selection(raw"[/\\]\.?js[/\\]".r, Function.const(true))
}

class Selection(platform: Regex, scalaVersionCheck: String => Boolean) extends Projection(platform, scalaVersionCheck) {
  def apply(scalaVersionCheck: String => Boolean) = new Projection(platform, scalaVersionCheck)
  def !(scalaVersionCheck: String => Boolean) = new Projection(platform, !scalaVersionCheck(_))
}

class Projection(platform: Regex, scalaVersionCheck: String => Boolean) {
  def orEmpty[V, T, C[U] <: TraversableLike[U, C[U]]](initialize: Def.Initialize[V])(
      implicit make: MakeCollection[V, T, C], cbf: CanBuildFrom[C[T], T, C[T]]) = Def.setting {
    if (scalaVersionCheck(scalaVersion.value) && platform.findAllIn(unmanagedBase.value.getPath).nonEmpty)
      make(initialize.value, cbf)
    else
      cbf().result()
  }

  def orPlatformCompileTimeStubs[V, T, C[U] <: TraversableLike[U, C[U]]](initialize: Def.Initialize[V])(
      implicit make: MakeCollection[V, ModuleID, C], cbf: CanBuildFrom[C[ModuleID], ModuleID, C[ModuleID]]) = Def.setting {
    if (!scalaVersionCheck(scalaVersion.value))
      cbf().result()
    else if (platform.findAllIn(unmanagedBase.value.getPath).nonEmpty)
      make(initialize.value, cbf)
    else
      make(initialize.value, cbf) map { _.withConfigurations(Some("provided")) }
  }
}

trait MakeCollection[V, T, C[U] <: TraversableLike[U, C[U]]] {
  def apply(v: V, cbf: CanBuildFrom[Nothing, T, C[T]]): C[T]
}

sealed trait MakeCollectionSeq {
  implicit def seq[T]: MakeCollection[T, T, Seq] = (v, cbf) => Seq(v)
}

sealed trait MakeCollectionValue extends MakeCollectionSeq {
  implicit def value[T, C[U] <: TraversableLike[U, C[U]]]: MakeCollection[T, T, C] = (v, cbf) => (cbf() += v).result()
}

object MakeCollection extends MakeCollectionValue {
  implicit def collection[T, C[U] <: TraversableLike[U, C[U]]]: MakeCollection[C[T], T, C] = (v, cbf) => v
}
