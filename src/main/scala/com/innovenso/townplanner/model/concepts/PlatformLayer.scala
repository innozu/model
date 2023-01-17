package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties.{
  CanAddProperties,
  CanConfigureDescription,
  CanConfigureTitle,
  HasDescription,
  HasTitle,
  Property
}
import com.innovenso.townplanner.model.language.{Concept, HasModelComponents}
import com.innovenso.townplanner.model.meta._

case class PlatformLayer(
    key: Key = Key("platform layer"),
    sortKey: SortKey = SortKey.next,
    order: Int = 0,
    color: Color = Color.random,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends Concept
    with HasDescription {
  val modelComponentType: ModelComponentType = ModelComponentType(
    "Platform Layer",
    classOf[PlatformLayer]
  )
  val aspect: Aspect = NoStructure
  val layer: Layer = OtherLayer

  def withProperty(property: Property): PlatformLayer =
    copy(properties = this.properties + (property.key -> property))
}

trait HasPlatformLayers extends HasModelComponents {
  def platformLayers: List[PlatformLayer] = components(classOf[PlatformLayer])
  def platformLayer(key: Key): Option[PlatformLayer] =
    component(key, classOf[PlatformLayer])
}

case class PlatformLayerConfigurer(
    modelComponent: PlatformLayer,
    propertyAdder: CanAddProperties
) extends CanConfigureTitle[PlatformLayer]
    with CanConfigureDescription[PlatformLayer] {
  def as(
      body: PlatformLayerConfigurer => Any
  ): PlatformLayer = {
    body.apply(this)
    propertyAdder.townPlan
      .platformLayer(modelComponent.key)
      .get
  }
}

trait CanAddPlatformLayers extends CanAddProperties {
  def describes(platformLayer: PlatformLayer): PlatformLayerConfigurer =
    PlatformLayerConfigurer(has(platformLayer), this)
}
