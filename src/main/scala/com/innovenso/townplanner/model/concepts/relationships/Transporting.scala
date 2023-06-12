package com.innovenso.townplanner.model.concepts.relationships

import com.innovenso.townplanner.model.concepts.properties.CanAddProperties
import com.innovenso.townplanner.model.concepts.properties.Property
import com.innovenso.townplanner.model.concepts.properties.Title
import com.innovenso.townplanner.model.meta.Key

case class Transporting(
    key: Key = Key("transporting"),
    source: Key,
    target: Key,
    bidirectional: Boolean = false,
    properties: Map[Key, Property] = Map(
      Key.fromString("title") -> Title("transports")
    )
) extends Relationship {
  val relationshipType: RelationshipType = RelationshipType(
    "transporting",
    "An accessing relationship represents that a data object is accessed by an application component."
  )
  val sourceTrait: Class[CanTransport] = classOf[CanTransport]
  val targetTrait: Class[CanBeTransported] = classOf[CanBeTransported]

  def withProperty(property: Property): Transporting =
    copy(properties = this.properties + (property.key -> property))
}

trait CanTransport extends CanBeRelationshipSource

trait CanBeTransported extends CanBeRelationshipTarget

trait CanConfigureTransportingSource[ModelComponentType <: CanTransport] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def transports(
      target: CanBeTransported,
      title: String
  ): Relationship =
    relationshipAdder.hasRelationship(
      Transporting(
        source = modelComponent.key,
        target = target.key
      ).withTitle(Title(title)).asInstanceOf[Transporting]
    )
}

trait CanConfigureTransportingTarget[ModelComponentType <: CanBeTransported] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def isTransportedBy(
      target: CanTransport,
      title: String
  ): Relationship =
    relationshipAdder.hasRelationship(
      Transporting(
        source = target.key,
        target = modelComponent.key
      ).withTitle(Title(title)).asInstanceOf[Transporting]
    )
}
