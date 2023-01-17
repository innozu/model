package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.meta.{Key, SortKey}

case class Title(
    value: String
) extends Property {
  val key: Key = Key.fromString("title")
  val sortKey: SortKey = SortKey.next
  val canBePlural: Boolean = true

  override def toString: String = value
}

trait HasTitle extends HasProperties {
  def title: Title = prop(Key.fromString("title"), classOf[Title]).getOrElse(Title(""))
  def withTitle(title: Title): HasProperties =
    withProperty(title)
}

trait CanConfigureTitle[ModelComponentType <: HasTitle] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def has(title: Title): ModelComponentType =
    propertyAdder.withProperty(modelComponent, title)

  def isTitled(title: String): ModelComponentType = has(Title(title))
}
