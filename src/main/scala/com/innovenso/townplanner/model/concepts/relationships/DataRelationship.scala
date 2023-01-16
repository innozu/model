package com.innovenso.townplanner.model.concepts.relationships

import com.innovenso.townplanner.model.concepts.properties.{
  CanAddProperties,
  Property
}
import com.innovenso.townplanner.model.meta.Key

case class DataRelationship(
    key: Key = Key("data relationship"),
    source: Key,
    target: Key,
    title: String = "has",
    left: Cardinality = One,
    right: Cardinality,
    bidirectional: Boolean = false,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends Relationship {
  val relationshipType: RelationshipType = RelationshipType(
    "data",
    "A data relationship between data objects."
  )
  val sourceTrait: Class[CanHaveDataRelationship] =
    classOf[CanHaveDataRelationship]
  val targetTrait: Class[CanHaveDataRelationship] =
    classOf[CanHaveDataRelationship]

  def withProperty(property: Property): DataRelationship =
    copy(properties = this.properties + (property.key -> property))
}

trait CanHaveDataRelationship
    extends CanBeRelationshipSource
    with CanBeRelationshipTarget

trait CanConfigureDataRelationships[
    ModelComponentType <: CanHaveDataRelationship
] {
  def relationshipAdder: CanAddRelationships
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def has(
      target: CanHaveDataRelationship,
      left: Cardinality,
      right: Cardinality,
      title: String
  ): Relationship =
    relationshipAdder.hasRelationship(
      DataRelationship(
        source = modelComponent.key,
        target = target.key,
        title = title,
        left = left,
        right = right
      )
    )
}

trait Cardinality {
  def value: String

  override def toString: String = value
}

case object One extends Cardinality {
  val value = "1"
}

case object ZeroOrOne extends Cardinality {
  val value = "0..1"
}

case object ZeroOrMore extends Cardinality {
  val value = "0..*"
}

case object OneOrMore extends Cardinality {
  val value = "1..*"
}
