package retier
package impl

private final case class ControlledIssuedValueImpl[P <: Remote[Peer], T]
    (f: P => T) extends ControlledIssuedValueImplBase[P, T] {
  def apply(p: P): T = f(p)
}

private final case class IssuedValueImpl[P <: Remote[Peer], T]
    (v: T) extends IssuedValueImplBase[P, T] {
  def apply(p: P): T = v
}

object ControlledIssuedValue {
  def create[P <: Remote[Peer], T](f: P => T): P <=> T =
    ControlledIssuedValueImpl(f)
}

object IssuedValue {
  def create[P <: Remote[Peer], T](v: T): P <-> T =
    IssuedValueImpl(v)

  def value[P <: Remote[Peer], T](v: P <-> T): T =
    v match {
      case IssuedValueImpl(v) => v
      case _ => throw new RetierImplementationError(
        s"invalid issued value (<->) implementation: ${className(v)}")
    }
}
