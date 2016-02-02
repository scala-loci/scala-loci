package retier
package impl

import java.util.concurrent.locks.ReentrantLock

final class FairSync {
  private[this] val lock = new ReentrantLock(true)

  def apply[T](body: => T): T = {
    lock.lock
    try body
    finally lock.unlock
  }
}
