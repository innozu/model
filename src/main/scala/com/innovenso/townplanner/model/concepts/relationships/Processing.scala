package com.innovenso.townplanner.model.concepts.relationships

import com.innovenso.townplanner.model.concepts.properties.{CanAddProperties, Property, Title}
import com.innovenso.townplanner.model.meta.Key

case class Processing(
    key: Key = Key("processing"),
    source: Key,
    target: Key,
    bidirectional: Boolean = false,
    properties: Map[Key, Property] = Map(
      Key.fromString("title") -> Title("processes")
    )
) extends Relationship {
  val relationshipType: RelationshipType = RelationshipType(
    "processing",
    "A processing relationship represents that a data object is processed by a system."
  )
  val sourceTrait: Class[CanProcess] = classOf[CanProcess]
  val targetTrait: Class[CanBeProcessed] = classOf[CanBeProcessed]
  def withProperty(property: Property): Processing =
    copy(properties = this.properties + (property.key -> property))
}

trait CanProcess extends CanBeRelationshipSource

trait CanBeProcessed extends CanBeRelationshipTarget

trait CanConfigureProcessingSource[ModelComponentType <: CanProcess] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def processes(
      target: CanBeProcessed,
      title: String
  ): Relationship =
    relationshipAdder.hasRelationship(
      Processing(
        source = modelComponent.key,
        target = target.key
      ).withTitle(Title(title)).asInstanceOf[Processing]
    )
}

trait CanConfigureProcessingTarget[ModelComponentType <: CanBeProcessed] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def isProcessedBy(
      target: CanProcess,
      title: String
  ): Relationship =
    relationshipAdder.hasRelationship(
      Processing(
        source = target.key,
        target = modelComponent.key
      ).withTitle(Title(title)).asInstanceOf[Processing]
    )
}
