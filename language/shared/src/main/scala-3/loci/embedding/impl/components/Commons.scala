package loci
package embedding
package impl
package components

import scala.quoted.*

trait Commons:
  this: Component =>
  import quotes.reflect.*

  object symbols:
    val `language.per` = Symbol.requiredPackage("loci.language.package$package").typeMember("per")
    val `language.on` = Symbol.requiredPackage("loci.language.package$package").typeMember("on")
    val `embedding.on` = Symbol.requiredPackage("loci.embedding.package$package").typeMember("on")
    val `Placed.on` = Symbol.requiredPackage("loci.embedding.Placed").typeMember("on")
    val `Placed.Subjective.on` = Symbol.requiredPackage("loci.embedding.Placed.Subjective").typeMember("on")
    val on = TypeRepr.of[Placement.On[?]].typeSymbol
    val placed = TypeRepr.of[Placed.type].typeSymbol
    val subjective = TypeRepr.of[Placed.Subjective[?, ?]].typeSymbol
    val function1 = TypeRepr.of[Function1[?, ?]].typeSymbol
    val contextResultCount = TypeRepr.of[annotation.internal.ContextResultCount].typeSymbol

  object types:
    val placed = TypeRepr.of[Placed[?, ?]]
    val remote = TypeRepr.of[language.Remote[?]]
    val conversion = TypeRepr.of[Conversion[?, ?]]

  object names:
    val sbj = "sbj"
    val body = "body"
end Commons
