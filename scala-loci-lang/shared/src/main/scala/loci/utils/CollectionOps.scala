package loci.utils

import java.time.LocalDateTime
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentLinkedQueue
import scala.annotation.tailrec

object CollectionOps {

  implicit class ConcurrentLinkedQueueOps[T <: { val timestamp: LocalDateTime }](queue: ConcurrentLinkedQueue[T]) {

    def addAndLimit(element: T, capacity: Int): Boolean = {
      require(capacity > 0)
      ConcurrentLinkedQueueOps(queue).limit(capacity - 1)
      queue.offer(element)
    }

    def addAndLimit(element: T, oldestTimestamp: LocalDateTime): Boolean = {
      while (Option(queue.peek()).exists(_.timestamp.isBefore(oldestTimestamp))) {
        queue.remove()
      }
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

  implicit class ConcurrentHashMapOps[K, V](map: ConcurrentHashMap[K, V]) {
    def getOrDefaultWithPut(key: K, default: V): V = {
      map.putIfAbsent(key, default)
      map.get(key)
    }
  }

}
