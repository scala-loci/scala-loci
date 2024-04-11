package loci
package embedding

import loci.language.*

import scala.annotation.StaticAnnotation

class multitier extends StaticAnnotation:
  def this(accessorGeneration: AccessorGeneration) = this()
