package retier


protected sealed trait ValueProxy extends Any


// placed types

sealed trait `local on`[+T, P <: Peer] extends Any with ValueProxy

sealed trait `shared on`[+T, P <: Peer] extends Any with ValueProxy
  with `local on`[T, P]


// captured type

sealed trait Captured[+T] extends Any with ValueProxy


// remote reference type

sealed trait Remote[+P <: Peer] extends Any

private trait RemoteImplBase[+P <: Peer] extends Remote[P]


// selected remote types

sealed trait `from single`[+T, P <: Peer] extends Any

sealed trait `from multiple`[+T, P <: Peer] extends Any

private trait SingleSelectionImplBase[+T, P <: Peer]
  extends (T `from single` P)

private trait MultipleSelectionImplBase[+T, P <: Peer]
  extends (T `from multiple` P)


// issued types

sealed trait <=>[-P <: Remote[Peer], +T] extends (P => T)

sealed trait <->[-P <: Remote[Peer], +T] extends (P <=> T)

private trait ControlledIssuedValueImplBase[-P <: Remote[Peer], +T]
  extends (P <=> T)

private trait IssuedValueImplBase[-P <: Remote[Peer], +T]
  extends (P <-> T)
