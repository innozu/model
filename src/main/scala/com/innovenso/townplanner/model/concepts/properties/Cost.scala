package com.innovenso.townplanner.model.concepts.properties

import java.util.Currency

import com.innovenso.townplanner.model.meta._
import com.innovenso.townplanner.model.samples

case class Capex(
    sortKey: SortKey = SortKey.next,
    title: String,
    description: String = "",
    category: Category = Category(None),
    fiscalYear: Year = ThisYear,
    numberOfUnits: UnitCount = UnitCount(1),
    unitOfMeasure: UnitOfMeasure = UnitOfMeasure("units"),
    costPerUnit: MonetaryAmount
) extends Cost

case class Opex(
    sortKey: SortKey = SortKey.next,
    title: String,
    description: String = "",
    category: Category = Category(None),
    fiscalYear: Year = ThisYear,
    numberOfUnits: UnitCount = UnitCount(1),
    unitOfMeasure: UnitOfMeasure = UnitOfMeasure("units"),
    costPerUnit: MonetaryAmount
) extends Cost

trait Cost extends Property {
  val key: Key = Key("cost")
  val canBePlural: Boolean = true
  def title: String
  def description: String
  def category: Category
  def fiscalYear: Year
  def numberOfUnits: UnitCount
  def unitOfMeasure: UnitOfMeasure
  def costPerUnit: MonetaryAmount
  def totalCost: MonetaryAmount = MonetaryAmount(
    numberOfUnits.count * costPerUnit.amount,
    costPerUnit.currency
  )
}

object Cost {
  def randomCapex: Capex = Capex(
    title = samples.title,
    description = samples.description,
    category = Category.random,
    fiscalYear = Year.random,
    numberOfUnits = UnitCount.random,
    unitOfMeasure = UnitOfMeasure.random,
    costPerUnit = MonetaryAmount.random
  )
  def randomOpex: Opex = Opex(
    title = samples.title,
    description = samples.description,
    category = Category.random,
    fiscalYear = Year.random,
    numberOfUnits = UnitCount.random,
    unitOfMeasure = UnitOfMeasure.random,
    costPerUnit = MonetaryAmount.random
  )

  def random: Cost = samples.randomInt(2) match {
    case 1 => randomCapex
    case 2 => randomOpex
  }

  def randoms: List[Cost] = samples.times(6, i => random)
}

trait HasCosts extends HasProperties {
  def costs(fiscalYear: Year): List[Cost] =
    costs.filter(_.fiscalYear == fiscalYear)

  def costs: List[Cost] = props(classOf[Cost])

  def costFiscalYears: List[Year] =
    costs.map(_.fiscalYear).distinct.sortWith(_.value < _.value)

  def totalCapex(fiscalYear: Year, currency: Currency): MonetaryAmount =
    MonetaryAmount(
      capex(fiscalYear)
        .filter(_.costPerUnit.currency == currency)
        .map(_.totalCost.amount)
        .sum,
      currency
    )

  def capex(fiscalYear: Year): List[Capex] =
    capex.filter(_.fiscalYear == fiscalYear)

  def capex: List[Capex] = props(classOf[Capex])

  def totalOpex(fiscalYear: Year, currency: Currency): MonetaryAmount =
    MonetaryAmount(
      opex(fiscalYear)
        .filter(_.costPerUnit.currency == currency)
        .map(_.totalCost.amount)
        .sum,
      currency
    )

  def opex(fiscalYear: Year): List[Opex] =
    opex.filter(_.fiscalYear == fiscalYear)

  def opex: List[Opex] = props(classOf[Opex])
}

trait CanConfigureCosts[
    ModelComponentType <: HasCosts
] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def costs(cost: Cost): ModelComponentType =
    propertyAdder.withProperty(modelComponent, cost)
}
