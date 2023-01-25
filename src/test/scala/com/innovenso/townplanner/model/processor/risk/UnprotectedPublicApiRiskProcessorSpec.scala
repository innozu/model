package com.innovenso.townplanner.model.processor.risk

import com.innovenso.townplanner.model.concepts._
import com.innovenso.townplanner.model.concepts.properties._
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class UnprotectedPublicApiRiskProcessorSpec
    extends AnyFlatSpec
    with GivenWhenThen {
  "unprotected API" should "result in risks being created" in new EnterpriseArchitectureContext {
    Given("platforms and their systems")
    val platform1: ItPlatform = ea hasRandom ItPlatform ()
    val platform2: ItPlatform = ea hasRandom ItPlatform ()
    val system1: ItSystem = ea describesRandom ItSystem() as { it =>
      it isPartOf platform1
    }
    val system2: ItSystem = ea describesRandom ItSystem() as { it =>
      it isPartOf platform2
    }
    val system3: ItSystem = ea describesRandom ItSystem() as { it =>
      it isPartOf platform2
    }
    And("containers with API")
    val container1: ItContainer =
      ea describes Microservice() as { it =>
        it has Title("API without authentication")
        it has API(
          scope = PublicScope(),
          authentication = NoAuthentication(),
          style = RestAPI()
        )
        it isPartOf system1
      }
    val container2: ItContainer =
      ea describes Microservice() as { it =>
        it has Title("API without rate limiting")
        it has API(
          scope = PublicScope(),
          rateLimiting = NoRateLimiting(),
          style = RestAPI()
        )
        it isPartOf system2
      }
    val container3: ItContainer =
      ea describes Microservice() as { it =>
        it has Title("API without DDoS protection")
        it has API(
          scope = PublicScope(),
          ddoSProtection = NoDDosProtection(),
          style = RestAPI()
        )
        it isPartOf system2
      }

    When("the Unprotected Public API Risk processor is run")
    process(UnprotectedPublicApiRiskProcessor()(ea))
    Then("risks are created for the 3 unprotected API")
    assert(townPlan.risks.size == 9)
  }
}
