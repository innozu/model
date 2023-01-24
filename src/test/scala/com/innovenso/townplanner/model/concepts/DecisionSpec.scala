package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties._
import com.innovenso.townplanner.model.meta.Key
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class DecisionSpec extends AnyFlatSpec with GivenWhenThen {
  "Decisions" can "be added to the town plan" in new EnterpriseArchitectureContext {
    val innovenso: Enterprise = ea hasRandomEnterprise ()
    val jurgen: Person = ea hasRandomPerson ()
    val virginie: Person = ea hasRandomPerson ()
    val paymentsCapability: BusinessCapability =
      ea hasRandomBusinessCapability { it =>
        it serves innovenso
      }
    val psp: ArchitectureBuildingBlock = ea hasRandomArchitectureBuildingBlock {
      it =>
        it realizes paymentsCapability
    }
    val paypal: ItSystem = ea hasRandomItSystem { it =>
      it realizes psp
    }

    val pspSelection: Decision =
      ea describes Decision() as { it =>
        it has Title("PSP Vendor Selection")
        it has Description(
          "one vendor should be chosen to handle all our payments"
        )
        it has CurrentState(description =
          "1. we have direct integrations with a number of payment methods"
        )
        it has CurrentState(description =
          "2. we spend a lot of money on maintaining the integrations and keeping them secure"
        )
        it has Goal(description = "to have one single PSP to integrate")
        it has Goal(description =
          "to be able to support all possible payment methods"
        )
        it has Assumption(description =
          "We are only looking for a solution for the online platform, not for the physical stores"
        )
        it serves innovenso
        it has FunctionalRequirement(
          key = Key.fromString("psp_wip"),
          title = "Support Vendor-Initiated Payments",
          weight = ShouldHave
        )
        it has FunctionalRequirement(
          key = Key.fromString("acquiring"),
          title = "Have a acquiring license",
          weight = MustHave
        )
        it has FunctionalRequirement(title = "Something else")
        it has QualityAttributeRequirement(
          title = "Availability",
          sourceOfStimulus = "a user",
          stimulus = "tries to make a payment",
          response = "the PSP handles the payment",
          responseMeasure = "99.99% of the time"
        )
        it has Constraint(title = "We only work with European companies")
        it has Constraint(title = "The budget of course")
        it changes paymentsCapability
        it keeps psp
        it removes paypal
        it hasStakeholder jurgen
        it isResponsibilityOf virginie
        it hasInformed jurgen
        it hasConsulted jurgen
        it dealsWith PCICompliance(
          "no payment data should be transported over our network, or stored on our servers"
        )
        it has ExtremelyHighImpact on Confidentiality(description =
          "payment data!"
        )
      }

    val adyen: DecisionOption =
      ea describes DecisionOption() as { it =>
        it has Title("AdYen")
        it isPartOf pspSelection
        it scores ExceedsExpectations(description =
          "very nice"
        ) on "psp_wip" of pspSelection
        it scores DoesNotMeetExpectations() on "acquiring" of pspSelection
      }

    assert(exists(pspSelection))
    assert(
      townPlan.decision(pspSelection.key).get.functionalRequirements.size == 3
    )
    assert(townPlan.decision(pspSelection.key).get.constraints.size == 2)
    assert(
      townPlan
        .decision(pspSelection.key)
        .get
        .qualityAttributeRequirements
        .size == 1
    )
    assert(
      townPlan
        .decision(pspSelection.key)
        .get
        .currentState
        .head
        .description
        .startsWith("1.")
    )

    assert(townPlan.options(pspSelection).size == 1)
    assert(townPlan.functionalRequirementScores(adyen).size == 3)
    assert(townPlan.qualityAttributeRequirementScores(adyen).size == 1)
    assert(townPlan.constraintScores(adyen).size == 2)
    assert(townPlan.rejectedOptions(pspSelection).size == 1)
    assert(townPlan.chosenOptions(pspSelection).isEmpty)
    assert(townPlan.optionsUnderInvestigation(pspSelection).isEmpty)
    assert(
      townPlan
        .decision(pspSelection.key)
        .exists(_.pciComplianceConcerns.nonEmpty)
    )
    assert(
      townPlan
        .decision(pspSelection.key)
        .exists(_.confidentialityImpact.nonEmpty)
    )
  }
}
