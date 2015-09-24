package retier


// placed types

sealed trait localOn[+T, P <: Peer] extends Any

sealed trait sharedOn[+T, P <: Peer] extends Any
  with (T localOn P)


// remote reference type

sealed trait Remote[+P <: Peer] extends Any with Equals

private trait RemoteImplBase[+P <: Peer] extends Remote[P]


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


// issued types

sealed trait <=>[-P <: Remote[Peer], +T] extends (P => T)

sealed trait <->[-P <: Remote[Peer], +T] extends (P <=> T)

private trait ControlledIssuedValueImplBase[-P <: Remote[Peer], +T]
  extends (P <=> T)

private trait IssuedValueImplBase[-P <: Remote[Peer], +T]
  extends (P <-> T)
