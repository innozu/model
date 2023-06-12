package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.concepts.PlatformLayer
import com.innovenso.townplanner.model.meta.Key
import com.innovenso.townplanner.model.meta.SortKey

case class PlatformLayerProperty(
    key: Key = Key("layer"),
    sortKey: SortKey = SortKey.next,
    platformLayerKey: Key
) extends Property {
  val canBePlural: Boolean = false
}

object PlatformLayerProperty {
  def apply(platformLayer: PlatformLayer): PlatformLayerProperty =
    new PlatformLayerProperty(platformLayerKey = platformLayer.key)
}

trait HasPlatformLayerProperties extends HasProperties {
  def platformLayer: Option[PlatformLayerProperty] = props(
    classOf[PlatformLayerProperty]
  ).headOption
  def withPlatformLayer(platformLayer: PlatformLayer): HasProperties =
    withProperty(PlatformLayerProperty(platformLayer))
  def withPlatformLayer(
      platformLayerProperty: PlatformLayerProperty
  ): HasProperties = withProperty(
    platformLayerProperty
  )
  def withPlatformLayer(key: Key): HasProperties = withProperty(
    PlatformLayerProperty(platformLayerKey = key)
  )
  def isOnPlatformLayer(layer: PlatformLayer): Boolean =
    platformLayer.exists(plp => plp.platformLayerKey == layer.key)

  def isOnPlatformLayer(key: Key): Boolean =
    platformLayer.exists(plp => plp.platformLayerKey == key)

  def isNotOnPlatformLayer: Boolean = platformLayer.isEmpty
}

trait CanConfigurePlatformLayerProperties[
    ModelComponentType <: HasPlatformLayerProperties
] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def has(platformLayer: PlatformLayer): HasPlatformLayerProperties =
    propertyAdder.withProperty(
      modelComponent,
      PlatformLayerProperty(platformLayer)
    )
  def isOn(platformLayer: PlatformLayer): HasPlatformLayerProperties =
    propertyAdder.withProperty(
      modelComponent,
      PlatformLayerProperty(platformLayer)
    )

  def isOn(key: Key): HasPlatformLayerProperties = propertyAdder.withProperty(
    modelComponent,
    PlatformLayerProperty(platformLayerKey = key)
  )
}
