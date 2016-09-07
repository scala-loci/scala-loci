package retier

trait Feature {
  sealed class NoImplicitConversionBridge
}

object feature extends Feature {
  implicit val noImplicitConversionBridge = new NoImplicitConversionBridge
}
