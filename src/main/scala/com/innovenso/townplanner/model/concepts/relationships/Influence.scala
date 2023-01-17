package com.innovenso.townplanner.model.concepts.relationships

import com.innovenso.townplanner.model.concepts.properties.{CanAddProperties, Property, Title}
import com.innovenso.townplanner.model.meta.Key

case class Influence(
    key: Key = Key("influencing"),
    source: Key,
    target: Key,
    bidirectional: Boolean = false,
    properties: Map[Key, Property] = Map(
      Key.fromString("title") -> Title("influences")
    )
) extends Relationship {
  val relationshipType: RelationshipType = RelationshipType(
    "influences",
    "The influences relationship represents that a element influences another element. For example, a business stakeholder can influence a decision."
  )
  val sourceTrait: Class[CanInfluence] = classOf[CanInfluence]
  val targetTrait: Class[CanBeInfluenced] = classOf[CanBeInfluenced]
  def withProperty(property: Property): Influence =
    copy(properties = this.properties + (property.key -> property))
}

trait CanInfluence extends CanBeRelationshipSource
trait CanBeInfluenced extends CanBeRelationshipTarget

trait CanConfigureInfluenceSource[ModelComponentType <: CanInfluence] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def isInfluencing(
      target: CanBeInfluenced,
      title: String = "influences"
  ): RelationshipConfigurer = RelationshipConfigurer(
    influences(target, title),
    propertyAdder,
    relationshipAdder
  )

  def influences(
      target: CanBeInfluenced,
      title: String = "influences"
  ): Relationship =
    relationshipAdder.hasRelationship(
      Influence(source = modelComponent.key, target = target.key)
        .withTitle(Title(title))
        .asInstanceOf[Influence]
    )
}

trait CanConfigureInfluenceTarget[ModelComponentType <: CanBeInfluenced] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def isBeingInfluencedBy(
      target: CanInfluence,
      title: String = "influences"
  ): RelationshipConfigurer = RelationshipConfigurer(
    isInfluencedBy(target, title),
    propertyAdder,
    relationshipAdder
  )

  def isInfluencedBy(
      target: CanInfluence,
      title: String = "influences"
  ): Relationship =
    relationshipAdder.hasRelationship(
      Influence(source = target.key, target = modelComponent.key)
        .withTitle(Title(title))
        .asInstanceOf[Influence]
    )
}
