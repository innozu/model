package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.meta.{Key, SortKey}
import com.innovenso.townplanner.model.samples

case class ExternalId(
    id: String,
    externalSystemName: String
) extends Property {
  val key: Key = Key("external ID")
  val sortKey: SortKey = SortKey.next
  val canBePlural: Boolean = true
}

object ExternalId {
  def apply(
      id: String,
      externalSystemName: String
  ): ExternalId = new ExternalId(id, externalSystemName)

  def random: ExternalId = new ExternalId(samples.id, samples.word)
  def randoms: List[ExternalId] = samples.times(5, i => random)
}

trait HasExternalIds extends HasProperties {
  def externalIds: List[ExternalId] = props(classOf[ExternalId])
  def externalId(key: Key): Option[ExternalId] =
    prop(key, classOf[ExternalId])
  def withExternalId(externalId: ExternalId): HasProperties =
    withProperty(externalId)
}

case class ExternalIdConfigurer(
    modelComponent: HasExternalIds,
    externalId: String,
    propertyAdder: CanAddProperties
) {
  def on(externalSystemName: String): HasExternalIds =
    propertyAdder.withProperty(
      modelComponent,
      ExternalId(externalId, externalSystemName)
    )

}

trait CanConfigureExternalIds[ModelComponentType <: HasExternalIds] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def isIdentifiedAs(externalId: String): ExternalIdConfigurer =
    ExternalIdConfigurer(modelComponent, externalId, propertyAdder)
}
