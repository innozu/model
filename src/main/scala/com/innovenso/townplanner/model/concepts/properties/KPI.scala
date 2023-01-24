package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.meta.{Key, SortKey}
import com.innovenso.townplanner.model.samples

case class KPI(
    key: Key,
    sortKey: SortKey,
    title: String,
    description: String
) extends Property {
  val canBePlural: Boolean = true
}

object KPI {
  def apply(
      key: Key = Key("kpi"),
      sortKey: SortKey = SortKey.next,
      title: String,
      description: String
  ): KPI = new KPI(key, sortKey, title, description)

  def random: KPI =
    apply(title = samples.title, description = samples.description)

  def randoms: List[KPI] = samples.times(5, i => random)
}

trait HasKPI extends HasProperties {
  def kpi: List[KPI] =
    props(classOf[KPI])
}

trait CanConfigureKPI[
    ModelComponentType <: HasKPI
] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def has(kpi: KPI): ModelComponentType =
    propertyAdder.withProperty(modelComponent, kpi)
}
