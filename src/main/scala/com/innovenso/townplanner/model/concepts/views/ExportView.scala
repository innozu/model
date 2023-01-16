package com.innovenso.townplanner.model.concepts.views

import com.innovenso.townplanner.model.concepts.properties.Property
import com.innovenso.townplanner.model.language.TimelessView
import com.innovenso.townplanner.model.meta._

case class ExportView(
    title: String = "Export",
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends TimelessView {
  val key: Key = Key("export")
  val sortKey: SortKey = SortKey.next
  val layer: Layer = OtherLayer
  val modelComponentType: ModelComponentType =
    ModelComponentType("Export", classOf[ExportView])
}
