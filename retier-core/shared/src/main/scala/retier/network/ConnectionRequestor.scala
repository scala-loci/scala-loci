package retier
package network

import scala.concurrent.Future

trait ConnectionRequestor {
  def request: Future[Connection]
}
