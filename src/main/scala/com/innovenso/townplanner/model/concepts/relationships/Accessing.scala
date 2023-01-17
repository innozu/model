package com.innovenso.townplanner.model.concepts.relationships

import com.innovenso.townplanner.model.concepts.properties.{
  CanAddProperties,
  Property,
  Title
}
import com.innovenso.townplanner.model.meta.Key

case class Accessing(
    key: Key = Key("accessing"),
    source: Key,
    target: Key,
    bidirectional: Boolean = false,
    properties: Map[Key, Property] = Map(
      Key.fromString("title") -> Title("accesses")
    )
) extends Relationship {
  val relationshipType: RelationshipType = RelationshipType(
    "accessing",
    "An accessing relationship represents that a data object is accessed by a system."
  )
  val sourceTrait: Class[CanAccess] = classOf[CanAccess]
  val targetTrait: Class[CanBeAccessed] = classOf[CanBeAccessed]
  def withProperty(property: Property): Accessing =
    copy(properties = this.properties + (property.key -> property))
}

trait CanAccess extends CanBeRelationshipSource

trait CanBeAccessed extends CanBeRelationshipTarget

trait CanConfigureAccessingSource[ModelComponentType <: CanAccess] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def accesses(
      target: CanBeAccessed,
      title: String
  ): Relationship =
    relationshipAdder.hasRelationship(
      Accessing(
        source = modelComponent.key,
        target = target.key
      ).withTitle(Title(title)).asInstanceOf[Accessing]
    )
}

trait CanConfigureAccessingTarget[ModelComponentType <: CanBeAccessed] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def isAccessedBy(
      target: CanAccess,
      title: String
  ): Relationship =
    relationshipAdder.hasRelationship(
      Accessing(
        source = target.key,
        target = modelComponent.key
      ).withTitle(Title(title)).asInstanceOf[Accessing]
    )
}
