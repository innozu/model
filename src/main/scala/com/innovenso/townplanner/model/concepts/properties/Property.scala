package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.language.{CanAddModelComponents, ModelComponent}
import com.innovenso.townplanner.model.meta.{Key, SortKey}

trait Property {
  def key: Key
  def sortKey: SortKey
  def canBePlural: Boolean
}

trait HasProperties extends ModelComponent {
  def properties: Map[Key, Property]
  def withProperty(property: Property): HasProperties
  def withProperties(properties: List[Property]): HasProperties = if (
    properties.isEmpty
  ) this
  else this.withProperty(properties.head).withProperties(properties.tail)

  def props[PropertyType <: Property](
      shouldBeOfClass: Class[PropertyType]
  ): List[PropertyType] =
    properties.values
      .filter(property => is(property, shouldBeOfClass))
      .map(property => as(property, shouldBeOfClass))
      .toList
      .sortWith(_.sortKey < _.sortKey)

  private def is(
      property: Property,
      shouldBeOfClass: Class[_ <: Property]
  ): Boolean = shouldBeOfClass.isInstance(property)

  private def as[PropertyType <: Property](
      property: Property,
      shouldBeOfClass: Class[PropertyType]
  ): PropertyType = shouldBeOfClass.cast(property)

  def prop[PropertyType <: Property](
      key: Key,
      shouldBeOfClass: Class[PropertyType]
  ): Option[PropertyType] =
    properties
      .get(key)
      .filter(property => is(property, shouldBeOfClass))
      .map(property => as(property, shouldBeOfClass))
}

trait CanAddProperties extends CanAddModelComponents {

  def hasProperty(modelComponentKey: Key, property: Property): HasProperties = {
    val modelComponentOption: Option[HasProperties] =
      townPlan.component(modelComponentKey, classOf[HasProperties])
    if (modelComponentOption.isEmpty)
      throw new IllegalArgumentException(
        s"the town plan does not contain a model component with key ${modelComponentKey.value} that can have properties"
      )
    else {
      withProperty(modelComponentOption, property)
    }
  }

  def withProperty[ModelComponentType <: HasProperties](
      modelComponent: ModelComponentType,
      property: Property
  ): ModelComponentType = {
    val modelComponentOption: Option[ModelComponentType] =
      townPlan.component(modelComponent.key, modelComponent.getClass)
    if (modelComponentOption.isEmpty)
      throw new IllegalArgumentException(
        s"the town plan does not contain a model component with key ${modelComponent.key.value} that ${modelComponent.getClass.getSimpleName}"
      )
    else {
      withProperty(modelComponentOption, property)
    }
  }

  def withProperty[ModelComponentType <: HasProperties](
      modelComponentOption: Option[ModelComponentType],
      property: Property
  ): ModelComponentType = {
    if (modelComponentOption.isEmpty)
      throw new IllegalArgumentException(
        s"trying to add property $property to a non-existant model component"
      )
    else if (
      modelComponentOption.get
        .props(property.getClass)
        .nonEmpty && !property.canBePlural
    )
      throw new IllegalArgumentException(
        s"the component ${modelComponentOption.get.key.value} already has a property of type ${property.getClass.getSimpleName}"
      )
    else {
      val updatedModelComponent =
        modelComponentOption.get.withProperty(property)
      this.townPlan = townPlan.copy(
        townPlan.modelComponents + (modelComponentOption.get.key -> updatedModelComponent)
      )
      this.townPlan
        .component(updatedModelComponent.key, modelComponentOption.get.getClass)
        .get
    }
  }

}
