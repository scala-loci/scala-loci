package loci


// placed types

sealed trait PlacedValue extends Any

sealed trait RemoteValue extends Any with PlacedValue

sealed trait localOn[+T, P <: Peer] extends Any
  with PlacedValue

sealed trait sharedOn[+T, P <: Peer] extends Any
  with RemoteValue with (T localOn P)

object RemoteValue extends ImplicitTransmissions.RemoteValue

object localOn extends ImplicitConversions.LocalOn

object sharedOn extends ImplicitConversions.SharedOn


// subjective types

sealed trait <=>[-P <: Remote[Peer], +T] extends Any

sealed trait <->[-P <: Remote[Peer], +T] extends Any


// selected remote types

sealed trait from[+T, P <: Peer] extends Any with RemoteValue

sealed trait fromSingle[+T, P <: Peer] extends Any with RemoteValue

sealed trait fromMultiple[+T, P <: Peer] extends Any with RemoteValue

private trait SelectionImplBase[+T, P <: Peer]
  extends (T from P)

private trait SingleSelectionImplBase[+T, P <: Peer]
  extends (T fromSingle P)

private trait MultipleSelectionImplBase[+T, P <: Peer]
  extends (T fromMultiple P)
