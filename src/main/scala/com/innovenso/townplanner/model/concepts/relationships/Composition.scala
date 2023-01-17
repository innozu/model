package com.innovenso.townplanner.model.concepts.relationships

import com.innovenso.townplanner.model.concepts.properties.{CanAddProperties, Property, Title}
import com.innovenso.townplanner.model.meta.Key

case class Composition(
    key: Key = Key("composition"),
    source: Key,
    target: Key,
    bidirectional: Boolean = false,
    properties: Map[Key, Property] = Map(
      Key.fromString("title") -> Title("is composed of")
    )
) extends Relationship {
  val relationshipType: RelationshipType = RelationshipType(
    "composition",
    "The composition relationship represents that an element consists of one or more other concepts."
  )
  val sourceTrait: Class[CanBeComposedOf] = classOf[CanBeComposedOf]
  val targetTrait: Class[CanCompose] = classOf[CanCompose]
  def withProperty(property: Property): Composition =
    copy(properties = this.properties + (property.key -> property))
}

trait CanBeComposedOf extends CanBeRelationshipSource
trait CanCompose extends CanBeRelationshipTarget

trait CanConfigureCompositionSource[ModelComponentType <: CanBeComposedOf] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def contains(target: CanCompose, title: String = ""): RelationshipConfigurer =
    RelationshipConfigurer(
      isComposedOf(target, title),
      propertyAdder,
      relationshipAdder
    )

  def isComposedOf(
      target: CanCompose,
      title: String = ""
  ): Relationship =
    relationshipAdder.hasRelationship(
      Composition(
        source = modelComponent.key,
        target = target.key
      ).withTitle(Title(title)).asInstanceOf[Composition]
    )
}

trait CanConfigureCompositionTarget[ModelComponentType <: CanCompose] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def belongsTo(
      target: CanBeComposedOf,
      title: String = ""
  ): RelationshipConfigurer =
    RelationshipConfigurer(
      isPartOf(target, title),
      propertyAdder,
      relationshipAdder
    )

  def isPartOf(target: CanBeComposedOf): Relationship =
    isPartOf(target, "")

  def isPartOf(
      target: CanBeComposedOf,
      title: String
  ): Relationship =
    relationshipAdder.hasRelationship(
      Composition(
        source = target.key,
        target = modelComponent.key
      ).withTitle(Title(title)).asInstanceOf[Composition]
    )
}
