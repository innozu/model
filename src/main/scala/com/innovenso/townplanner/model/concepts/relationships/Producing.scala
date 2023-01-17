package com.innovenso.townplanner.model.concepts.relationships

import com.innovenso.townplanner.model.concepts.properties.{CanAddProperties, Property, Title}
import com.innovenso.townplanner.model.meta.Key

case class Producing(
    key: Key = Key("producing"),
    source: Key,
    target: Key,
    bidirectional: Boolean = false,
    properties: Map[Key, Property] = Map(
      Key.fromString("title") -> Title("produces")
    )
) extends Relationship {
  val relationshipType: RelationshipType = RelationshipType(
    "produces",
    "A producing relationship represents that a data object is produced by a system."
  )
  val sourceTrait: Class[CanProduce] = classOf[CanProduce]
  val targetTrait: Class[CanBeProduced] = classOf[CanBeProduced]

  def withProperty(property: Property): Producing =
    copy(properties = this.properties + (property.key -> property))
}

trait CanProduce extends CanBeRelationshipSource

trait CanBeProduced extends CanBeRelationshipTarget

trait CanConfigureProducingSource[ModelComponentType <: CanProduce] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def produces(
      target: CanBeProduced,
      title: String
  ): Relationship =
    relationshipAdder.hasRelationship(
      Producing(
        source = modelComponent.key,
        target = target.key
      ).withTitle(Title(title)).asInstanceOf[Producing]
    )
}

trait CanConfigureProducingTarget[ModelComponentType <: CanBeProduced] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def isProducedBy(
      target: CanProduce,
      title: String
  ): Relationship =
    relationshipAdder.hasRelationship(
      Producing(
        source = target.key,
        target = modelComponent.key
      ).withTitle(Title(title)).asInstanceOf[Producing]
    )
}
