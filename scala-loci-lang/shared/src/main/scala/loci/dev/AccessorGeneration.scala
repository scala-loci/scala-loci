package loci.dev

sealed trait AccessorGeneration

object AccessorGeneration {
  // generate accessors for all declared values or inherited values without an accessor
  // - that are accessed remotely in the current module
  case object Deferred extends AccessorGeneration

  // generate accessors for all declared values or inherited values without an accessor
  // - that are accessed remotely in the current module
  // - that are not accessed remotely in the current module if accessor creation is possible
  //   (in particular, it might fail for type-parametric values)
  case object Preferred extends AccessorGeneration

  // generate accessors for all declared values or inherited values without an accessor
  case object Required extends AccessorGeneration

  // generate accessors for all declared values or inherited values
  // even if inherited values already have an associated accessor
  case object Forced extends AccessorGeneration
}
