package loci
package transmitter
package dev

class TransmitterResolutionException(evidence: String, unspecified: String)
  extends IllegalArgumentException(
    s"`$evidence` evidence provided, but no `$unspecified` is specified. " +
    "If the evidence was inferred by the compiler, " +
    "this hints at an internal issue of the multitier implementation.")
