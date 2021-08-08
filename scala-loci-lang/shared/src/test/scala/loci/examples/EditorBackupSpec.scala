package loci
package examples

import communicator.NetworkListener
import transmitter.Serializables._
import transmitter.transmittable.TransformingTransmittable

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.concurrent.Future


case class Data(value: String = "")

object Data {
  implicit val transmittable: TransformingTransmittable[Data, String, Data] = TransformingTransmittable(
    provide = (value, context) => value.value,
    receive = (value, context) => Data(value))
}


@multitier trait BackupService {
  @peer type Processor <: { type Tie <: Single[Storage] }
  @peer type Storage <: { type Tie <: Multiple[Processor] }

  def store(id: Long, data: Data): Unit on Processor
  def load(id: Long): Future[Data] on Processor
}

@multitier trait MemoryBackup extends BackupService {
  @peer type Processor <: { type Tie <: Single[Storage] }
  @peer type Storage <: { type Tie <: Multiple[Processor] }

  def store(id: Long, data: Data): Unit on Processor = on[Storage].run.capture(id, data) { map += id -> data }
  def load(id: Long): Future[Data] on Processor = on[Storage].run.capture(id) { map(id) }.asLocal

  private val map = on[Storage] local { mutable.Map.empty[Long, Data].withDefaultValue(Data()) }
}

@multitier trait Editor {
  import contexts.Immediate.Implicits.global

  @peer type Client <: backup.Processor { type Tie <: Single[Server] with Single[backup.Storage] }
  @peer type Server <: backup.Storage { type Tie <: Multiple[Client] with Multiple[backup.Processor] }

  def exportBackup(id: Long) = on[Client] local { backup.store(id, data) }
  def importBackup(id: Long) = on[Client] local { backup.load(id) foreach { data = _ } }

  var data: Local[Data] on Client = Data()

  val backup: MemoryBackup
}

@multitier object editorApp extends Editor {
  @multitier object backup extends MemoryBackup
}


class EditorBackupSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Editor backup example"

  it should "run correctly" in {
    val listener = new NetworkListener

    val serverInstance = multitier start new Instance[editorApp.Server](
      contexts.Immediate.global,
      listen[editorApp.Client] { listener })

    val clientInstance0 = multitier start new Instance[editorApp.Client](
      contexts.Immediate.global,
      connect[editorApp.Server] { listener.createConnector() })

    val clientInstance1 = multitier start new Instance[editorApp.Client](
      contexts.Immediate.global,
      connect[editorApp.Server] { listener.createConnector() })

    clientInstance0.instance.current map { _ retrieve editorApp.data } should be (Some(Data()))
    clientInstance1.instance.current map { _ retrieve editorApp.data } should be (Some(Data()))

    clientInstance0.instance.current foreach { _ retrieve (editorApp.data = Data("test")) }

    clientInstance0.instance.current map { _ retrieve editorApp.data } should be (Some(Data("test")))
    clientInstance1.instance.current map { _ retrieve editorApp.data } should be (Some(Data()))

    clientInstance0.instance.current foreach { _ retrieve editorApp.exportBackup(5L) }
    clientInstance0.instance.current foreach { _ retrieve editorApp.importBackup(7L) }

    clientInstance0.instance.current map { _ retrieve editorApp.data } should be (Some(Data()))
    clientInstance1.instance.current map { _ retrieve editorApp.data } should be (Some(Data()))

    clientInstance0.instance.current foreach { _ retrieve editorApp.importBackup(5L) }
    clientInstance1.instance.current foreach { _ retrieve editorApp.importBackup(5L) }

    clientInstance0.instance.current map { _ retrieve editorApp.data } should be (Some(Data("test")))
    clientInstance1.instance.current map { _ retrieve editorApp.data } should be (Some(Data("test")))

    clientInstance1.terminate()
    clientInstance0.terminate()
    serverInstance.terminate()
  }
}
