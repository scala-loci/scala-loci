package loci
package runtime

import scala.util.control.NonFatal
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext

trait Dispatch[D <: Dispatch[D]] extends Runnable {
  def blockedBy(dispatch: D): Boolean

  final def blockedBy(dispatches: TraversableOnce[D]): Boolean =
    dispatches exists blockedBy
}

trait Undispatchable[D <: Dispatch[D]] { this: Dispatch[D] =>
  final def blockedBy(dispatch: D) = false
  final def run() = { }
}

class Dispatcher[D <: Dispatch[D]](implicit context: ExecutionContext) {
  private val dispatches = ListBuffer.empty[(D, Boolean)]

  def dispatch(dispatch: D*): Unit = dispatches synchronized {
    dispatch foreach { dispatches += _ -> false }
    next(Seq.empty)
  }

  def ignoreDispatched(dispatch: D*): Unit = dispatches synchronized {
    dispatch foreach { dispatches -= _ -> false }
    next(Seq.empty)
  }

  private def next(executed: Traversable[D]): Unit = dispatches synchronized {
    executed foreach { dispatches -= _ -> true }

    val pendings = dispatches collect { case (dispatch, true) => dispatch }

    val dispatchings = ListBuffer.empty[ListBuffer[D]]

    dispatches transform { case (dispatch, running) =>
      dispatch match {
        case _: Undispatchable[D] =>
          pendings += dispatch
          dispatch -> false

        case _ =>
          if (!running) {
            if (!(dispatch blockedBy pendings)) {
              dispatchings filter dispatch.blockedBy match {
                case ListBuffer() =>
                  dispatchings += ListBuffer(dispatch)
                  dispatch -> true

                case ListBuffer(dispatching) =>
                  dispatching += dispatch
                  dispatch -> true

                case _ =>
                  pendings += dispatch
                  dispatch -> false
              }
            }
            else {
              pendings += dispatch
              dispatch -> false
            }
          }
          else
            dispatch -> true
      }
    }

    dispatchings foreach { dispatching =>
      logging.tracing(context) execute new Runnable {
        def run() = {
          var throwable: Throwable = null

          dispatching foreach { dispatch =>
            try dispatch.run()
            catch {
              case NonFatal(exception) =>
                if (throwable == null)
                  throwable = exception
                else
                  throwable.addSuppressed(exception)
            }
          }

          next(dispatching)

          if (throwable != null)
            throw throwable
        }
      }
    }
  }
}
