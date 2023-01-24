package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties._
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class ItSystemSpec extends AnyFlatSpec with GivenWhenThen {
  "IT Systems" can "be added to the town plan" in new EnterpriseArchitectureContext {
    Given("a platform")
    val platform: ItPlatform = ea hasRandomItPlatform ()
    And("some layers")
    val green: PlatformLayer = ea hasRandomPlatformLayer ()
    val red: PlatformLayer = ea hasRandomPlatformLayer ()

    When("a system is described")
    val itSystem: ItSystem = ea describes ItSystem() as { it =>
      it has Title("The System")
      it has Description("It does things")
      it should BeInvestedIn()
      it ratesFailureAs Catastrophic(consequences = "people die")
      it provides ResilienceMeasure("circuit breaker")
      it isPartOf platform
      it isIdentifiedAs "abc" on "Sparx"
      it isOn green
    }

    Then("the system exists")
    assert(exists(itSystem))
    And("the system has the right relationships")
    assert(townPlan.relationships.size == 1)
    And("the system has the correct criticality")
    assert(townPlan.system(itSystem.key).get.isCatastrophicCriticality)
    And("the system has the correct external IDs")
    assert(townPlan.system(itSystem.key).exists(_.externalIds.nonEmpty))
    And("the system is on the correct platform layer")
    assert(
      townPlan
        .system(itSystem.key)
        .exists(_.platformLayer.exists(p => p.platformLayerKey == green.key))
    )
  }

}
