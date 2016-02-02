package retier
package impl
package engine

sealed trait Aggregation[T, R] extends Function1[Aggregator[T], Aggregator[R]] {
  type InputAggregator = Aggregator[T]
  type OutputAggregator = Aggregator[R]
  def apply(aggregator: InputAggregator): OutputAggregator
}

object Aggregation {
  def apply[T, R](f: Aggregator[T] => Aggregator[R]) =
    new Aggregation[T, R] {
      def apply(aggregator: InputAggregator): OutputAggregator = f(aggregator)
    }
}


sealed trait AugmentedAggregation[T, R] extends Aggregation[T, T with R] {
  override type InputAggregator = Aggregator[T]
  override type OutputAggregator = Aggregator[T with R]
  def apply(aggregator: InputAggregator): OutputAggregator
}

object AugmentedAggregation {
  def apply[T, R](f: Aggregator[T] => Aggregator[T with R]) =
    new AugmentedAggregation[T, R] {
      def apply(aggregator: InputAggregator): OutputAggregator = f(aggregator)
    }
}


sealed trait UniformAggregation[T] extends Aggregation[T, T] {
  override type InputAggregator = Aggregator[T]
  override type OutputAggregator = Aggregator[T]
  def apply(aggregator: InputAggregator): OutputAggregator
}

object UniformAggregation {
  def apply[T](f: Aggregator[T] => Aggregator[T]) =
    new UniformAggregation[T] {
      def apply(aggregator: InputAggregator): OutputAggregator = f(aggregator)
    }
}
