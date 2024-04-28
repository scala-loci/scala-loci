package loci
package embedding
package impl
package components

import scala.annotation.experimental

@experimental
trait RemoteAccessorGeneration:
  this: Component & RemoteAccessorSynthesis =>
  import quotes.reflect.*

  def addAccessors(module: ClassDef): ClassDef =
    val accessors = synthesizeAccessors(module.symbol)

    val (_, identifierDefinition) = accessors.identifier
    val (_, signatureDefinition) = accessors.signature

    val peersDefinitions = accessors.peers.values flatMap: (_, signatureDefinition, _, tiesDefinition) =>
      signatureDefinition.toList ++ tiesDefinition.toList
    val marshallingDefinitions = accessors.overridden.iterator ++ accessors.marshalling.iterator flatMap: (_, definition) =>
      definition.toList
    val placedDefinitions = accessors.placed.values flatMap: (_, definition) =>
      definition.toList

    val definitions =
      identifierDefinition.toList ++
      signatureDefinition.toList ++
      peersDefinitions.toList ++
      marshallingDefinitions.toList ++
      placedDefinitions.toList

    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, definitions ++ module.body)
  end addAccessors
end RemoteAccessorGeneration
