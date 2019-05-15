package loci
package runtime

import loci.Testing._

@multitier object ServerClientApp {
  @peer type Server <: { type Tie <: Multiple[Client] }
  @peer type Client <: { type Tie <: Single[Server] }

  val id = on[Client] { implicit! => 0 }

  def square(a: Int) = on[Server] { implicit! => a * a }
}
