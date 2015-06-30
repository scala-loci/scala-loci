package retier

sealed trait Transmitter

trait PushBasedTransmitter extends Transmitter

trait PullBasedTransmitter extends Transmitter
