package com.innovenso.townplanner.model.concepts.relationships

import com.innovenso.townplanner.model.concepts.properties.{
  CanAddProperties,
  Property
}
import com.innovenso.townplanner.model.meta.Key

case class Owning(
    key: Key = Key("owning"),
    source: Key,
    target: Key,
    title: String = "owns",
    bidirectional: Boolean = false,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends Relationship {
  val relationshipType: RelationshipType = RelationshipType(
    "owning",
    "An owning relationship represents that a data object is owned by a system."
  )
  val sourceTrait: Class[CanOwn] = classOf[CanOwn]
  val targetTrait: Class[CanBeOwned] = classOf[CanBeOwned]
  def withProperty(property: Property): Owning =
    copy(properties = this.properties + (property.key -> property))
}

trait CanOwn extends CanBeRelationshipSource

trait CanBeOwned extends CanBeRelationshipTarget

trait CanConfigureOwningSource[ModelComponentType <: CanOwn] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def owns(
      target: CanBeOwned,
      title: String
  ): Relationship =
    relationshipAdder.hasRelationship(
      Owning(
        source = modelComponent.key,
        target = target.key,
        title = title
      )
    )
}

trait CanConfigureOwningTarget[ModelComponentType <: CanBeOwned] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def isOwnedBy(
      target: CanOwn,
      title: String
  ): Relationship =
    relationshipAdder.hasRelationship(
      Owning(
        source = target.key,
        target = modelComponent.key,
        title = title
      )
    )
}
