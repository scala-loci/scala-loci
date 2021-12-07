package loci
package runtime

class Instance[P](
    val values: PlacedValues,
    val remoteConnections: RemoteConnections)
  extends language.Instance[P](0) {

  private val doTerminated = Notice.Steady[Unit]

  remoteConnections.terminated foreach { _ => doTerminated.trySet() }

  if (remoteConnections.isTerminated)
    doTerminated.trySet()

  def terminate(): Unit = remoteConnections.terminate()

  val terminated: Notice.Steady[Unit] = doTerminated.notice
}
