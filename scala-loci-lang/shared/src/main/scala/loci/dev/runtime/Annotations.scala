package loci.dev
package runtime

import scala.annotation.StaticAnnotation
import scala.annotation.meta._

@getter @setter @beanGetter @beanSetter @companionClass @companionMethod
final class AbstractValue extends StaticAnnotation

@getter @setter @beanGetter @beanSetter @companionClass @companionMethod
final class MultitierStub extends StaticAnnotation

final class MultitierModule extends StaticAnnotation
