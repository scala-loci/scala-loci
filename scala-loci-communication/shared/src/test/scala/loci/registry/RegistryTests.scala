package loci
package registry

import communicator.{Connector, Listener}
import transmitter.RemoteAccessException
import transmitter.Serializables._

import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.Success

object RegistryTests extends Matchers {
  type Test = (Listener[Connections.Protocol], Connector[Connections.Protocol], => Unit, => Unit, => Unit) => Unit

  def `handle binding and lookup correctly`(
      listener: Listener[Connections.Protocol],
      connector: Connector[Connections.Protocol],
      setupListener: => Unit = (),
      setupConnector: => Unit = (),
      cleanup: => Unit = ()): Unit = {
    var registry0: Registry = null
    var registry1: Registry = null

    try {
      val promise = Promise[(Int, String)]()

      registry0 = new Registry
      registry0.bind("future")(promise.future)
      registry0.bind("intfun")(() => (???): Int)
      registry0.listen(listener)

      setupListener

      registry1 = new Registry
      val futureRemote = registry1.connect(connector)

      setupConnector

      Await.ready(futureRemote, 1.minute)
      val remote = futureRemote.value.get.get

      val result0 = registry1.lookup[concurrent.Future[(Int, String)]]("future", remote)
      val result1 = registry1.lookup[() => Int]("intfun", remote)
      val futureValue = result0
      val intfunValue = result1()

      promise.success(5 -> "yay")

      Await.ready(futureValue, 1.minute)
      futureValue.value should be (Some(Success(5 -> "yay")))

      Await.ready(intfunValue, 1.minute)
      val remoteException = intercept[RemoteAccessException] { intfunValue.value.get.get }
      remoteException.reason should matchPattern { case RemoteAccessException.RemoteException("scala.NotImplementedError", _) => }
    }
    finally {
      registry0.terminate()
      registry1.terminate()
      cleanup
    }
  }

  def `handle subjective binding and lookup correctly`(
      listener: Listener[Connections.Protocol],
      connector: Connector[Connections.Protocol],
      setupListener: => Unit = (),
      setupConnector: => Unit = (),
      cleanup: => Unit = ()): Unit = {
    var registry0: Registry = null
    var registry1: Registry = null

    try {
      val events = mutable.ListBuffer.empty[String]

      val valueBinding = registry.Binding[String]("value")
      val methodBinding = registry.Binding[() => String]("method")

      def value(remote: transmitter.RemoteRef) = {
        events.synchronized { events += "value called" }
        "value result"
      }

      def method(remote: transmitter.RemoteRef) = {
        events.synchronized { events += "method called" }
        "method result"
      }

      registry0 = new Registry
      registry0.bindSbj(valueBinding)(value _)
      registry0.bindSbj(methodBinding)(method _)
      registry0.listen(listener)

      setupListener

      registry1 = new Registry
      val futureRemote = registry1.connect(connector)

      setupConnector

      Await.ready(futureRemote, 1.minute)
      val remote = futureRemote.value.get.get

      val result0 = registry1.lookup(valueBinding, remote)
      val result1 = registry1.lookup(methodBinding, remote)

      val result0a = result0 map { result => events.synchronized { events += result } }
      val result0b = result0 map { result => events.synchronized { events += result } }
      val result1a = result1() map { result => events.synchronized { events += result } }
      val result1b = result1() map { result => events.synchronized { events += result } }

      Await.ready(Future.sequence(Seq(result0a, result0b, result1a, result1b)), 1.minute)

      events should contain theSameElementsAs Seq(
        "value called",
        "value result",
        "value result",
        "method called",
        "method result",
        "method called",
        "method result")

      events filter { _ startsWith "value" } should contain theSameElementsInOrderAs Seq(
        "value called", "value result", "value result")

      events find { _ startsWith "method" } should contain ("method called")

      events.remove(events.indexOf("method called"))
      events.remove(events.indexOf("method result"))

      events filterNot { _ startsWith "value" } should contain theSameElementsInOrderAs Seq(
        "method called", "method result")
    }
    finally {
      registry0.terminate()
      registry1.terminate()
      cleanup
    }
  }
}
