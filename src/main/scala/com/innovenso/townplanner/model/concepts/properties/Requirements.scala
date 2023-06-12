package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.meta.Key
import com.innovenso.townplanner.model.meta.SortKey
import com.innovenso.townplanner.model.samples

case class Constraint(
    key: Key = Key("constraint"),
    sortKey: SortKey = SortKey.next,
    title: String,
    description: String = "",
    weight: RequirementWeight = ShouldHave
) extends Requirement

case class FunctionalRequirement(
    key: Key = Key("functional requirement"),
    sortKey: SortKey = SortKey.next,
    title: String,
    description: String = "",
    weight: RequirementWeight = ShouldHave
) extends Requirement

case class QualityAttributeRequirement(
    key: Key = Key("qar"),
    sortKey: SortKey = SortKey.next,
    title: String,
    sourceOfStimulus: String = "",
    stimulus: String = "",
    environment: String = "",
    response: String = "",
    responseMeasure: String = "",
    weight: RequirementWeight = ShouldHave
) extends Requirement {
  val description: String = response
  val descriptionList: List[(String, String)] =
    describe("Source of Stimulus", sourceOfStimulus) ++ describe(
      "Stimulus",
      stimulus
    ) ++ describe("Environment", environment) ++ describe(
      "Response",
      response
    ) ++ describe("Response Measure", responseMeasure)

  private def describe(title: String, value: String): List[(String, String)] =
    if (value.isEmpty || value.isBlank) Nil else List((title, value))

}

trait Requirement extends Property {
  val canBePlural: Boolean = true
  def title: String
  def description: String
  def weight: RequirementWeight
}

object Requirement {
  def randomFunctionalRequirement: FunctionalRequirement =
    FunctionalRequirement(
      title = samples.title,
      description = samples.description,
      weight = RequirementWeight.random
    )
  def randomQualityAttributeRequirement: QualityAttributeRequirement =
    QualityAttributeRequirement(
      title = samples.title,
      sourceOfStimulus = samples.title,
      stimulus = samples.title,
      response = samples.title,
      environment = samples.title,
      responseMeasure = samples.title,
      weight = RequirementWeight.random
    )

  def randomConstraint: Constraint = Constraint(
    title = samples.title,
    description = samples.description,
    weight = RequirementWeight.random
  )

  def random: Requirement = samples.randomInt(3) match {
    case 1 => randomFunctionalRequirement
    case 2 => randomQualityAttributeRequirement
    case 3 => randomConstraint
  }

  def randoms: List[Requirement] = samples.times(10, i => random)
}

trait HasRequirements extends HasProperties {
  def requirements: List[Requirement] = props(classOf[Requirement])
  def constraints: List[Constraint] = props(classOf[Constraint])
  def functionalRequirements: List[FunctionalRequirement] = props(
    classOf[FunctionalRequirement]
  )
  def qualityAttributeRequirements: List[QualityAttributeRequirement] = props(
    classOf[QualityAttributeRequirement]
  )
}

trait CanConfigureRequirements[
    ModelComponentType <: HasRequirements
] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def has(requirement: Requirement): ModelComponentType =
    propertyAdder.withProperty(modelComponent, requirement)
}

trait RequirementWeight {
  def name: String
  def description: String
  def weight: Int
}

object RequirementWeight {
  def random: RequirementWeight = samples.randomInt(5) match {
    case 1 => MustHave
    case 2 => ShouldHave
    case 3 => WouldHave
    case 4 => CouldHave
    case 5 => UnknownRequirementWeight
  }
}

case object MustHave extends RequirementWeight {
  val name: String = "Must Have"
  val description: String =
    "This requirement is non-negotiable. Any option that does not meet it should be rejected."
  val weight: Int = 21
}

case object ShouldHave extends RequirementWeight {
  val name: String = "Should Have"
  val description: String =
    "This requirement is very important and should weigh heavily on any decision."
  val weight: Int = 13
}

case object CouldHave extends RequirementWeight {
  val name: String = "Could Have"
  val description: String =
    "This requirement can be used to tip the balance in case multiple options meet the same must-haves and should-haves."
  val weight: Int = 8
}

case object WouldHave extends RequirementWeight {
  val name: String = "Would Have"
  val description: String =
    "Not really a requirement, but would be nice to have."
  val weight: Int = 5
}

case object UnknownRequirementWeight extends RequirementWeight {
  val name: String = "Unknown"
  val description: String =
    "The weight of this requirement has not been determined yet."
  val weight: Int = 1
}
