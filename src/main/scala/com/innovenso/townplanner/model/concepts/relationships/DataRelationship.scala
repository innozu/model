package com.innovenso.townplanner.model.concepts.relationships

import com.innovenso.townplanner.model.concepts.properties.CanAddProperties
import com.innovenso.townplanner.model.concepts.properties.Property
import com.innovenso.townplanner.model.concepts.properties.Title
import com.innovenso.townplanner.model.meta.Key

case class DataRelationship(
    key: Key = Key("data relationship"),
    source: Key,
    target: Key,
    left: Cardinality = One,
    right: Cardinality = ZeroOrMore,
    bidirectional: Boolean = false,
    properties: Map[Key, Property] = Map(
      Key.fromString("title") -> Title("has")
    )
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
        left = left,
        right = right
      ).withTitle(Title(title)).asInstanceOf[DataRelationship]
    )
}

trait Cardinality {
  def value: String

  override def toString: String = value
}

object Cardinality {
  def fromString(value: String): Cardinality =
    Option(value).map(_.toLowerCase).map(_.trim).getOrElse("") match {
      case "1" | "one"             => One
      case "0..1" | "zero or one"  => ZeroOrOne
      case "0..*" | "zero or more" => ZeroOrMore
      case "1..*" | "one or more"  => OneOrMore
      case _                       => ZeroOrMore
    }
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
