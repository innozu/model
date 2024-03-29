package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.meta.Key
import com.innovenso.townplanner.model.meta.SortKey
import com.innovenso.townplanner.model.samples

case class Description(
    value: String
) extends Property {
  val key: Key = Key("description")
  val sortKey: SortKey = SortKey.next
  val canBePlural: Boolean = true

  override def toString: String = value
}

object Description {
  def apply(value: String): Description = new Description(value)

  def random: Description = new Description(samples.description)
  def randoms: List[Description] =
    samples.times(5, i => random)
}

trait HasDescription extends HasProperties {
  def descriptions: List[Description] = props(classOf[Description])
  def description(key: Key): Option[Description] =
    prop(key, classOf[Description])
  def withDescription(description: Description): HasProperties =
    withProperty(description)
}

trait CanConfigureDescription[ModelComponentType <: HasDescription] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def has(description: Description): ModelComponentType =
    propertyAdder.withProperty(modelComponent, description)
}
