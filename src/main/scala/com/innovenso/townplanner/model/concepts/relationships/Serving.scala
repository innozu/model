package com.innovenso.townplanner.model.concepts.relationships

import com.innovenso.townplanner.model.concepts.properties.CanAddProperties
import com.innovenso.townplanner.model.concepts.properties.Property
import com.innovenso.townplanner.model.concepts.properties.Title
import com.innovenso.townplanner.model.meta.Key

case class Serving(
    key: Key = Key("serving"),
    source: Key,
    target: Key,
    bidirectional: Boolean = false,
    properties: Map[Key, Property] = Map(
      Key.fromString("title") -> Title("serves")
    )
) extends Relationship {
  val relationshipType: RelationshipType = RelationshipType(
    "serving",
    "The serving relationship represents that an element provides its functionality to another element."
  )
  val sourceTrait: Class[CanServe] = classOf[CanServe]
  val targetTrait: Class[CanBeServed] = classOf[CanBeServed]
  def withProperty(property: Property): Serving =
    copy(properties = this.properties + (property.key -> property))
}

trait CanServe extends CanBeRelationshipSource
trait CanBeServed extends CanBeRelationshipTarget

trait CanConfigureServingSource[ModelComponentType <: CanServe] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def isServing(
      target: CanBeServed,
      title: String = "serves"
  ): RelationshipConfigurer =
    RelationshipConfigurer(
      serves(target, title),
      propertyAdder,
      relationshipAdder
    )

  def serves(
      target: CanBeServed,
      title: String = "serves"
  ): Relationship =
    relationshipAdder.hasRelationship(
      Serving(
        source = modelComponent.key,
        target = target.key
      ).withTitle(Title(title)).asInstanceOf[Serving]
    )
}

trait CanConfigureServingTarget[ModelComponentType <: CanBeServed] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def isBeingServedBy(
      target: CanServe,
      title: String = "serves"
  ): RelationshipConfigurer =
    RelationshipConfigurer(
      isServedBy(target, title),
      propertyAdder,
      relationshipAdder
    )

  def isServedBy(
      target: CanServe,
      title: String = "serves"
  ): Relationship =
    relationshipAdder.hasRelationship(
      Serving(
        source = target.key,
        target = modelComponent.key
      ).withTitle(Title(title)).asInstanceOf[Serving]
    )
}
