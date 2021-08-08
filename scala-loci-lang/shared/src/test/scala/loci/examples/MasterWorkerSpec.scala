package loci
package examples

import communicator.NetworkListener
import transmitter.Serializables._
import transmitter.transmittable.TransformingTransmittable

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Future
import scala.util.Success


trait Task {
  type Result
  def exec(): Result
}

class IntTask extends Task {
  type Result = Int
  def exec() = 42
}

object IntTask {
  implicit val transmittable: TransformingTransmittable[IntTask, Unit, IntTask] = TransformingTransmittable(
    provide = (value, context) => (),
    receive = (value, context) => new IntTask)
}


@multitier trait MasterWorker[T <: Task] {
  @peer type Master
  @peer type Worker

  def run(task: T): Future[task.Result] on Master
}

@multitier trait SingleMasterWorker[T <: Task] extends MasterWorker[T] {
  @peer type Master <: { type Tie <: Single[Worker] }
  @peer type Worker <: { type Tie <: Single[Master] }

  def run(task: T) = on[Master] { (remote call execute(task)).asLocal }
  private def execute(task: T) = on[Worker] { task.exec() }
}


@multitier trait VolunteerProcessingReference {
  @peer type Client <: comp.Worker
  @peer type Server <: comp.Master

  val comp: MasterWorker[IntTask]

  val result = on[Server] local { comp.run(new IntTask) }
}

@multitier object masterWorkerReference extends VolunteerProcessingReference {
  @multitier object comp extends SingleMasterWorker[IntTask]
}


@multitier trait VolunteerProcessingMixin { this: MasterWorker[IntTask] =>
  @peer type Client <: Worker
  @peer type Server <: Master

  val result = on[Server] local { run(new IntTask) }
}

@multitier object masterWorkerMixin extends VolunteerProcessingMixin with SingleMasterWorker[IntTask]


class MasterWorkerSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Master/Worker example"

  it should "run correctly with multitier module references" in {
    val listener = new NetworkListener

    val serverInstance = multitier start new Instance[masterWorkerReference.Server](
      contexts.Immediate.global,
      listen[masterWorkerReference.Client] { listener })

    val clientInstance = multitier start new Instance[masterWorkerReference.Client](
      contexts.Immediate.global,
      connect[masterWorkerReference.Server] { listener.createConnector() })

    val result = serverInstance.instance map { instance =>
      instance retrieve masterWorkerReference.result
    }

    result.current flatMap { _.value } should be (Some(Success(42)))

    clientInstance.terminate()
    serverInstance.terminate()
  }

  it should "run correctly with multitier module mixing" in {
    val listener = new NetworkListener

    val serverInstance = multitier start new Instance[masterWorkerMixin.Server](
      contexts.Immediate.global,
      listen[masterWorkerMixin.Client] { listener })

    val clientInstance = multitier start new Instance[masterWorkerMixin.Client](
      contexts.Immediate.global,
      connect[masterWorkerMixin.Server] { listener.createConnector() })

    val result = serverInstance.instance map { _ retrieve masterWorkerMixin.result }

    result.current flatMap { _.value } should be (Some(Success(42)))

    clientInstance.terminate()
    serverInstance.terminate()
  }
}
