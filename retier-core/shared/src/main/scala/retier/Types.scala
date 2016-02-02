package retier


// placed types

sealed trait localOn[+T, P <: Peer] extends Any

sealed trait sharedOn[+T, P <: Peer] extends Any
  with (T localOn P)


// issued types

sealed trait <=>[-P <: Remote[Peer], +T] extends Any

sealed trait <->[-P <: Remote[Peer], +T] extends Any


// selected remote types

sealed trait from[+T, P <: Peer] extends Any

sealed trait fromSingle[+T, P <: Peer] extends Any

sealed trait fromMultiple[+T, P <: Peer] extends Any

private trait SelectionImplBase[+T, P <: Peer]
  extends (T from P)

private trait SingleSelectionImplBase[+T, P <: Peer]
  extends (T fromSingle P)

private trait MultipleSelectionImplBase[+T, P <: Peer]
  extends (T fromMultiple P)
