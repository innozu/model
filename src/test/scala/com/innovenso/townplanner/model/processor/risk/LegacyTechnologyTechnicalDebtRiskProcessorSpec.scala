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
    val platform1: ItPlatform = ea hasRandomItPlatform ()
    val platform2: ItPlatform = ea hasRandomItPlatform ()
    val system1: ItSystem = ea hasRandomItSystem { it =>
      it isPartOf platform1
    }
    val system2: ItSystem = ea hasRandomItSystem { it =>
      it isPartOf platform2
    }
    val system3: ItSystem = ea hasRandomItSystem { it =>
      it isPartOf platform2
    }
    And("some technologies")
    val tech1 = ea describes Language() as { it =>
      it has Title("deprecated language")
      it should BeEliminated()
    }
    val tech2 = ea describes Language() as { it =>
      it has Title("Modern Language")
      it should BeInvestedIn()
    }
    And("containers with API")
    val container1: ItContainer =
      ea describes Microservice() as { it =>
        it has Title("microservice")
        it isImplementedBy tech1
        it isImplementedBy tech2
        it isPartOf system1
      }
    val container2: ItContainer =
      ea describes Microservice() as { it =>
        it has Title("microservice 2")
        it isPartOf system2
        it isImplementedBy tech1
      }
    val container3: ItContainer =
      ea describes Microservice() as { it =>
        it has Title("microservice 3")
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
