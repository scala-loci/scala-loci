package loci.utils

import java.util.concurrent.ConcurrentLinkedQueue
import scala.annotation.tailrec

object CollectionOps {

  implicit class ConcurrentLinkedQueueOps[T](queue: ConcurrentLinkedQueue[T]) {

    def addAndLimit(element: T, capacity: Int): Boolean = {
      require(capacity > 0)
      ConcurrentLinkedQueueOps(queue).limit(capacity - 1)
      queue.offer(element)
    }

    @tailrec
    private def limit(n: Int): Unit = {
      if (queue.size() > n) {
        queue.remove()
        ConcurrentLinkedQueueOps(queue).limit(n)
      }
    }
  }

}