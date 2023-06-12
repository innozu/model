package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.meta.Key
import com.innovenso.townplanner.model.meta.SortKey
import com.innovenso.townplanner.model.samples
import com.innovenso.townplanner.model.samples.randomInt

abstract class Criticality extends Property {
  val key: Key = Key("criticality")
  val canBePlural: Boolean = false
  val sortKey: SortKey = SortKey.next
  def name: String
  def description: String
  def consequences: String

  def level: Char
}

case class Catastrophic(consequences: String = "") extends Criticality {
  val name = "Catastrophic"
  val level = 'A'
  val description =
    "a component failure would significantly affect the operability of the system/platform and could cause bankruptcy, law suits or even the loss of life"
}

case class Hazardous(consequences: String = "") extends Criticality {
  val name = "Hazardous"
  val level = 'B'
  val description =
    "an incident would pose a major threat to the company and its employees"
}

case class Major(consequences: String = "") extends Criticality {
  val name = "Major"
  val level = 'C'
  val description =
    "an incident will impact the working of the larger whole, resulting in outages and loss of money"
}

case class Minor(consequences: String = "") extends Criticality {
  val name = "Minor"
  val level = 'D'
  val description =
    "an incident will have a minor impact the working of the larger whole, not resulting in system outages, but a reduced customer experience"
}

case class NoEffect(consequences: String = "") extends Criticality {
  val name = "No Effect"
  val level = 'E'
  val description =
    "an incident will have no impact the working of the larger whole, and will not have any impact on the customer experience"
}

case class UnknownCriticality(consequences: String = "") extends Criticality {
  val name = "Unknown"
  val level = 'X'
  val description =
    "The criticality has not yet been determined"
}

object Criticality {
  def fromString(
      value: String,
      consequences: Option[String] = None
  ): Criticality = Option(value).map(_.toLowerCase).getOrElse("") match {
    case "no effect" | "noeffect" | "e" => NoEffect(consequences.getOrElse(""))
    case "minor" | "d"                  => Minor(consequences.getOrElse(""))
    case "major" | "c"                  => Major(consequences.getOrElse(""))
    case "hazardous" | "b"              => Hazardous(consequences.getOrElse(""))
    case "catastrophic" | "a" => Catastrophic(consequences.getOrElse(""))
    case _                    => UnknownCriticality(consequences.getOrElse(""))
  }

  def random: Criticality = randomInt(5) match {
    case 1 => Catastrophic(samples.description)
    case 2 => Hazardous(samples.description)
    case 3 => Major(samples.description)
    case 4 => Minor(samples.description)
    case 5 => NoEffect(samples.description)
  }
}

trait HasCriticality extends HasProperties {
  def isCatastrophicCriticality: Boolean =
    criticality.isInstanceOf[Catastrophic]

  def isHazardousCriticality: Boolean = criticality.isInstanceOf[Hazardous]

  def isMajorCriticality: Boolean = criticality.isInstanceOf[Major]

  def isMinorCriticality: Boolean = criticality.isInstanceOf[Minor]

  def criticality: Criticality =
    props(classOf[Criticality]).headOption
      .getOrElse(UnknownCriticality())

  def isNoEffectCriticality: Boolean = criticality.isInstanceOf[NoEffect]

  def isUnknownCriticality: Boolean =
    criticality.isInstanceOf[UnknownCriticality]

}

trait CanConfigureCriticality[
    ModelComponentType <: HasCriticality
] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def ratesFailureAs(criticality: Criticality): ModelComponentType =
    propertyAdder.withProperty(modelComponent, criticality)

  def ratesImpactAs(criticality: Criticality): ModelComponentType =
    propertyAdder.withProperty(modelComponent, criticality)
}
