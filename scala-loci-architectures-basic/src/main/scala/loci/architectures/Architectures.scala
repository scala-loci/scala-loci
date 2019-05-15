package loci
package architectures

@multitier trait Architecture {
  @peer type Node <: { type Tie <: Multiple[Node] }
}

@multitier trait P2P extends Architecture {
  @peer type Peer <: Node { type Tie <: Multiple[Peer] }
}

@multitier trait P2PRegistry extends P2P {
  @peer type Registry <: Node { type Tie <: Multiple[Peer] }
  @peer type Peer <: Node { type Tie <: Optional[Registry] with Multiple[Peer] }
}

@multitier trait MultiClientServer extends Architecture {
  @peer type Server <: Node { type Tie <: Multiple[Client] }
  @peer type Client <: Node { type Tie <: Single[Server] with Single[Node] }
}

@multitier trait ClientServer extends MultiClientServer {
  @peer type Server <: Node { type Tie <: Single[Client] }
  @peer type Client <: Node { type Tie <: Single[Server] with Single[Node] }
}

@multitier trait Ring extends Architecture {
  @peer type Node <: { type Tie <: Single[Prev] with Single[Next] }
  @peer type Prev <: Node
  @peer type Next <: Node
  @peer type RingNode <: Prev with Next
}
