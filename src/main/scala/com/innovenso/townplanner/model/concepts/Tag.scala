package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties._
import com.innovenso.townplanner.model.language.{Concept, HasModelComponents, ModelComponent}
import com.innovenso.townplanner.model.meta._

case class Tag(
    key: Key = Key("tag"),
    sortKey: SortKey = SortKey.next,
    color: Color = Color.random,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends Concept
    with HasDescription {
  val modelComponentType: ModelComponentType =
    ModelComponentType("Tag", classOf[Tag])
  val aspect: Aspect = NoStructure
  val layer: Layer = OtherLayer

  def withProperty(property: Property): Tag =
    copy(properties = this.properties + (property.key -> property))
}

trait HasTags extends HasModelComponents {
  def tags: List[Tag] = components(classOf[Tag])
  def tag(key: Key): Option[Tag] =
    component(key, classOf[Tag])
  def taggedComponents[ComponentClass <: ModelComponent with HasTagProperties](
      tag: Tag,
      shouldBeOfClass: Class[ComponentClass]
  ): List[ComponentClass] = components(shouldBeOfClass).filter(c =>
    c.tags.exists(tp => tp.tagKey == tag.key)
  )
  def tags(component: HasTagProperties): List[Tag] =
    component.tags.flatMap(tp => tag(tp.tagKey))
}

case class TagConfigurer(
    modelComponent: Tag,
    propertyAdder: CanAddProperties
) extends CanConfigureTitle[Tag]
    with CanConfigureDescription[Tag] {
  def as(
      body: TagConfigurer => Any
  ): Tag = {
    body.apply(this)
    propertyAdder.townPlan
      .tag(modelComponent.key)
      .get
  }
}

trait CanAddTags extends CanAddProperties {
  def describes(tag: Tag): TagConfigurer =
    TagConfigurer(has(tag), this)
}
