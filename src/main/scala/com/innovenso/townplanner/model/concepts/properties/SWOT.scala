package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.meta.Key
import com.innovenso.townplanner.model.meta.SortKey
import com.innovenso.townplanner.model.samples

abstract class SWOT extends Property {
  val key: Key = Key("swot")
  val canBePlural: Boolean = true
  val sortKey: SortKey = SortKey.next
  def name: String
  def description: String
}

case class Strength(description: String) extends SWOT {
  val name = "Strength"
}

case class Weakness(description: String) extends SWOT {
  val name = "Weakness"
}

case class Opportunity(description: String) extends SWOT {
  val name = "Opportunity"
}

case class Threat(description: String) extends SWOT {
  val name = "Threat"
}

object SWOT {
  def fromString(name: String, description: String): SWOT =
    Option(name).map(_.toLowerCase) match {
      case Some("strength")    => Strength(description)
      case Some("weakness")    => Weakness(description)
      case Some("opportunity") => Opportunity(description)
      case Some("threat")      => Threat(description)
      case _                   => Weakness(description)
    }

  def random: SWOT = samples.randomInt(4) match {
    case 1 => Strength(samples.description)
    case 2 => Weakness(samples.description)
    case 3 => Opportunity(samples.description)
    case 4 => Threat(samples.description)
  }

  def randoms: List[SWOT] = samples.times(10, i => random)
}

trait HasSWOT extends HasProperties {
  def swots: List[SWOT] =
    props(classOf[SWOT])
  def strengths: List[SWOT] =
    props(classOf[Strength])
  def weaknesses: List[Weakness] =
    props(classOf[Weakness])
  def opportunities: List[Opportunity] =
    props(classOf[Opportunity])
  def threats: List[Threat] =
    props(classOf[Threat])

}

trait CanConfigureSWOT[
    ModelComponentType <: HasSWOT
] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def has(swot: SWOT): ModelComponentType =
    propertyAdder.withProperty(modelComponent, swot)
}
