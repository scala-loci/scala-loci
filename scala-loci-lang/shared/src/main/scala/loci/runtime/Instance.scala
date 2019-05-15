package loci
package runtime

import scala.concurrent.{Future, Promise}

class Instance[P](
    val values: PlacedValues,
    val remoteConnections: RemoteConnections)
  extends loci.Instance[P](0) {

  private val doTerminated = Promise[Unit]

  remoteConnections.terminated notify { _ => doTerminated.trySuccess(()) }

  if (remoteConnections.isTerminated)
    doTerminated.trySuccess(())

  def terminate(): Unit = remoteConnections.terminate()

  val terminated: Future[Unit] = doTerminated.future
}
