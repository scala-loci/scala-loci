package loci
package language

import embedding.*

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted.*

@experimental
class multitier(using MultitierPreprocessor) extends MacroAnnotation:
  def this(accessorGeneration: AccessorGeneration)(using MultitierPreprocessor) = this()
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    impl.Multitier.annotation(tree)

//object multitier:
//  def start[P, Inst[P] <: Instance[P]](instance: Inst[P]): Runtime[P] =
//  macro impl.Instance.start
//
//  @compileTimeOnly("method can only be invoked in multitier code")
//  def running: Boolean = erased
//
//  @compileTimeOnly("method can only be invoked in multitier code")
//  def terminate(): Unit = erased
