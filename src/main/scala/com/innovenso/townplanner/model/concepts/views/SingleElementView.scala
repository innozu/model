package com.innovenso.townplanner.model.concepts.views

import com.innovenso.townplanner.model.concepts.properties.Property
import com.innovenso.townplanner.model.language.{Element, TimelessView}
import com.innovenso.townplanner.model.meta.{Key, Layer, ModelComponentType, SortKey}

case class SingleElementView(forElement: Element,
                              properties: Map[Key, Property] = Map.empty[Key, Property]) extends TimelessView {
  val key: Key = forElement.key
  val sortKey: SortKey = forElement.sortKey
  val title: String = forElement.title
  val layer: Layer = forElement.layer
  val modelComponentType: ModelComponentType = forElement.modelComponentType
}
