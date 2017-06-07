package loci

package object communicator {
  type ProtocolCommon = Protocol with SetupInfo with SecurityInfo with SymmetryInfo
}
