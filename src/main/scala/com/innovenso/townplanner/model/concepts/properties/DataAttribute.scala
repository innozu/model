package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.meta.{Key, SortKey}

case class DataAttribute(
    name: String,
    description: Option[String] = None,
    required: Boolean = false,
    multiple: Boolean = false,
    dataType: Option[String] = None
) extends Property {
  val key: Key = Key("data attribute")
  val sortKey: SortKey = SortKey.next
  val canBePlural: Boolean = true
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
