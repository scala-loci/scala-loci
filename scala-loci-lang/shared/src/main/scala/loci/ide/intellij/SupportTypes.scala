package loci
package ide.intellij

protected[loci] trait LocalValueTypes extends LocalValueTypesFastMacro {
  @annotation.compileTimeOnly(
    "Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$loci$intellij$localValueTypes[T, U, Dummy]
    (implicit
        ev0: IntelliJDummy,
        ev1: ValueTypes[T, _, Dummy, U]): loci.LocalValueTypes[T, U] = `#macro`(ev0, ev1)
}

protected[loci] trait RemoteValueTypes extends RemoteValueTypesFastMacro {
  @annotation.compileTimeOnly(
    "Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$loci$intellij$remoteValueTypes[T, R <: Remote[Peer], U, Dummy]
    (implicit
        ev0: IntelliJDummy,
        ev1: ValueTypes[T, R, U, Dummy]): loci.RemoteValueTypes[T, R, U] = `#macro`(ev0, ev1)
}
