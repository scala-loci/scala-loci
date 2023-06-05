package loci
package embedding

def erased: Nothing = erased()
def erased(ev: Any*): Nothing =
  throw new NotImplementedError("Erased language constructs should never be used")
