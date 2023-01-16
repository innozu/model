package com.innovenso.townplanner.model.processor.risk

import com.innovenso.townplanner.model.concepts._
import com.innovenso.townplanner.model.concepts.properties._
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class LegacyTechnologyTechnicalDebtRiskProcessorSpec
    extends AnyFlatSpec
    with GivenWhenThen {
  "legacy technologies being used" should "result in risks being created" in new EnterpriseArchitectureContext {
    Given("platforms and their systems")
    val platform1: ItPlatform = samples.platform(name = Some("platform 1"))
    val platform2: ItPlatform = samples.platform(name = Some("platform 2"))
    val system1: ItSystem =
      samples.system(
        withContainers = false,
        name = Some("system 1"),
        containingPlatform = Some(platform1)
      )
    val system2: ItSystem =
      samples.system(
        withContainers = false,
        name = Some("system 2"),
        containingPlatform = Some(platform2)
      )
    val system3: ItSystem =
      samples.system(
        withContainers = false,
        name = Some("system 3"),
        containingPlatform = Some(platform2)
      )
    And("some technologies")
    val tech1 = ea describes Language(title = "Deprecated Language") as { it =>
      it should BeEliminated()
    }
    val tech2 = ea describes Language(title = "Modern Language") as { it =>
      it should BeInvestedIn()
    }
    And("containers with API")
    val container1: ItContainer =
      ea describes Microservice(title = "implementing everything") as { it =>
        it isImplementedBy tech1
        it isImplementedBy tech2
        it isPartOf system1
      }
    val container2: ItContainer =
      ea describes Microservice(title = "implementing deprecated") as { it =>
        it isPartOf system2
        it isImplementedBy tech1
      }
    val container3: ItContainer =
      ea describes Microservice(title = "implementing modern") as { it =>
        it isPartOf system2
        it isImplementedBy tech2
      }

    When("the Legacy Technology Technical Debt Risk processor is run")
    process(LegacyTechnologyTechnicalDebtRiskProcessor()(ea))
    Then(
      "risks are created for the 3 containers implementing deprecated technologies"
    )
    assert(townPlan.risks.size == 2)
  }
}
