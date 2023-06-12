package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.meta.Key
import com.innovenso.townplanner.model.meta.SortKey
import com.innovenso.townplanner.model.samples

case class DataAttribute(
    name: String,
    description: Option[String],
    required: Boolean,
    multiple: Boolean,
    dataType: Option[String]
) extends Property {
  val key: Key = Key("data attribute")
  val sortKey: SortKey = SortKey.next
  val canBePlural: Boolean = true
}

object DataAttribute {
  def apply(
      name: String,
      description: Option[String] = None,
      required: Boolean = false,
      multiple: Boolean = false,
      dataType: Option[String] = None
  ): DataAttribute =
    new DataAttribute(name, description, required, multiple, dataType)

  def random: DataAttribute = new DataAttribute(
    samples.word,
    Some(samples.description),
    samples.randomBoolean,
    samples.randomBoolean,
    Some(samples.word)
  )

  def randoms: List[DataAttribute] = samples.times(10, i => random)
}

trait HasDataAttributes extends HasProperties {
  def dataAttributes: List[DataAttribute] = props(classOf[DataAttribute])
  def dataAttribute(key: Key): Option[DataAttribute] =
    prop(key, classOf[DataAttribute])
  def withDataAttribute(dataAttribute: DataAttribute): HasProperties =
    withProperty(dataAttribute)
}

trait CanConfigureDataAttributes[ModelComponentType <: HasDataAttributes] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def has(dataAttribute: DataAttribute): ModelComponentType =
    propertyAdder.withProperty(modelComponent, dataAttribute)
}
