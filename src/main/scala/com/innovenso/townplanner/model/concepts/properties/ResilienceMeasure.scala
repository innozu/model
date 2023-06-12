package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.meta.Key
import com.innovenso.townplanner.model.meta.SortKey
import com.innovenso.townplanner.model.samples

case class ResilienceMeasure(
    description: String
) extends Property {
  val key: Key = Key("resilience")
  val sortKey: SortKey = SortKey.next
  val canBePlural: Boolean = true
}

object ResilienceMeasure {
  def apply(description: String): ResilienceMeasure = new ResilienceMeasure(
    description
  )

  def random: ResilienceMeasure = apply(samples.description)
}

trait HasResilienceMeasures extends HasProperties {
  def resilienceMeasures: List[ResilienceMeasure] = props(
    classOf[ResilienceMeasure]
  )
  def resilienceMeasure(key: Key): Option[ResilienceMeasure] =
    prop(key, classOf[ResilienceMeasure])
  def withResilienceMeasure(
      resilienceMeasure: ResilienceMeasure
  ): HasProperties =
    withProperty(resilienceMeasure)
}

trait CanConfigureResilienceMeasures[
    ModelComponentType <: HasResilienceMeasures
] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def provides(resilienceMeasure: ResilienceMeasure): HasResilienceMeasures =
    propertyAdder.withProperty(modelComponent, resilienceMeasure)
}
