package com.innovenso.townplanner.model.concepts.relationships

import com.innovenso.townplanner.model.concepts.properties.CanAddProperties
import com.innovenso.townplanner.model.concepts.properties.Property
import com.innovenso.townplanner.model.concepts.properties.Title
import com.innovenso.townplanner.model.meta.Key

case class Realization(
    key: Key = Key("realizing"),
    source: Key,
    target: Key,
    bidirectional: Boolean = false,
    properties: Map[Key, Property] = Map(
      Key.fromString("title") -> Title("realizes")
    )
) extends Relationship {
  val relationshipType: RelationshipType = RelationshipType(
    "realization",
    "The realization relationship represents that an entity plays a critical role in the creation, achievement, sustenance, or operation of a more abstract entity."
  )
  val sourceTrait: Class[CanRealize] = classOf[CanRealize]
  val targetTrait: Class[CanBeRealized] = classOf[CanBeRealized]
  def withProperty(property: Property): Realization =
    copy(properties = this.properties + (property.key -> property))
}

trait CanRealize extends CanBeRelationshipSource
trait CanBeRealized extends CanBeRelationshipTarget

trait CanConfigureRealizationSource[ModelComponentType <: CanRealize] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def isRealizing(
      target: CanBeRealized,
      title: String = "realizes"
  ): RelationshipConfigurer =
    RelationshipConfigurer(
      realizes(target, title),
      propertyAdder,
      relationshipAdder
    )

  def realizes(
      target: CanBeRealized,
      title: String = "realizes"
  ): Relationship =
    relationshipAdder.hasRelationship(
      Realization(
        source = modelComponent.key,
        target = target.key
      ).withTitle(Title(title)).asInstanceOf[Realization]
    )
}

trait CanConfigureRealizationTarget[ModelComponentType <: CanBeRealized] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def isBeingRealizedBy(
      target: CanRealize,
      title: String = "realizes"
  ): RelationshipConfigurer =
    RelationshipConfigurer(
      isRealizedBy(target, title),
      propertyAdder,
      relationshipAdder
    )

  def isRealizedBy(
      target: CanRealize,
      title: String = "realizes"
  ): Relationship =
    relationshipAdder.hasRelationship(
      Realization(
        source = target.key,
        target = modelComponent.key
      ).withTitle(Title(title)).asInstanceOf[Realization]
    )
}
