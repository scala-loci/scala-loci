package retier
package transmission

trait AbstractionId extends Equals

trait AbstractionRef {
  def channel: Channel
  def derive(name: String): AbstractionRef
}
