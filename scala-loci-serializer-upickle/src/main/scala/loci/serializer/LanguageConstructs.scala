package loci.serializer

import _root_.upickle.default._
import loci.valueref.ValueRef

trait LanguageConstructs {

  implicit def rwValueRef[V, P]: ReadWriter[ValueRef[V, P]] = macroRW[ValueRef[V, P]]

  implicit def rwPeerSignature: ReadWriter[loci.runtime.Peer.Signature] = macroRW[loci.runtime.Peer.Signature]

  implicit def rwModuleSignature: ReadWriter[loci.runtime.Module.Signature] = macroRW[loci.runtime.Module.Signature]
}
