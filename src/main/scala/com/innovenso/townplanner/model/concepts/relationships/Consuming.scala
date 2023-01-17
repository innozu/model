package com.innovenso.townplanner.model.concepts.relationships

import com.innovenso.townplanner.model.concepts.properties.{CanAddProperties, Property, Title}
import com.innovenso.townplanner.model.meta.Key

case class Consuming(
    key: Key = Key("consuming"),
    source: Key,
    target: Key,
    bidirectional: Boolean = false,
    properties: Map[Key, Property] = Map(
      Key.fromString("title") -> Title("consumes")
    )
) extends Relationship {
  val relationshipType: RelationshipType = RelationshipType(
    "consumes",
    "A consuming relationship represents that a data object is consumed by a system."
  )
  val sourceTrait: Class[CanConsume] = classOf[CanConsume]
  val targetTrait: Class[CanBeConsumed] = classOf[CanBeConsumed]

  def withProperty(property: Property): Consuming =
    copy(properties = this.properties + (property.key -> property))
}

trait CanConsume extends CanBeRelationshipSource

trait CanBeConsumed extends CanBeRelationshipTarget

trait CanConfigureConsumingSource[ModelComponentType <: CanConsume] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def consumes(
      target: CanBeConsumed,
      title: String
  ): Relationship =
    relationshipAdder.hasRelationship(
      Consuming(
        source = modelComponent.key,
        target = target.key
      ).withTitle(Title(title)).asInstanceOf[Consuming]
    )
}

trait CanConfigureConsumingTarget[ModelComponentType <: CanBeConsumed] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def isConsumedBy(
      target: CanConsume,
      title: String
  ): Relationship =
    relationshipAdder.hasRelationship(
      Consuming(
        source = target.key,
        target = modelComponent.key
      ).withTitle(Title(title)).asInstanceOf[Consuming]
    )
}
