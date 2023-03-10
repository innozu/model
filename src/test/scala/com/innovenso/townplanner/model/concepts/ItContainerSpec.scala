package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties._
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class ItContainerSpec extends AnyFlatSpec with GivenWhenThen {
  "IT Containers" can "be added to the town plan" in new EnterpriseArchitectureContext {
    Given("a system")
    val itSystem: ItSystem = ea hasRandom ItSystem()
    And("a technology")
    val java: LanguageOrFramework = ea hasRandomTech Language()
    And("a delivery team")
    val team: Team = ea hasRandomActor Team()
    When("a microservice is added to the town plan")
    val ms: Microservice = ea describes Microservice() as { it =>
      it has Title("BFF")
      it has Description("Backend for Frontend")
      it should BeInvestedIn()
      it isPartOf itSystem
      it isImplementedBy java
      it isDeliveredBy team
      it has ApiDocumentation(url = "https://townplanner.be")
      it has API(
        authentication = NoAuthentication(),
        style = RestAPI(),
        scope = PublicScope(),
        ddoSProtection = NoDDosProtection(),
        rateLimiting = NoRateLimiting()
      )
    }
    And("a database is added to the town plan")
    val db: Database = ea describes Database() as { it =>
      it has Title("The Database")
      it isPartOf itSystem
      it isUsedBy ms
    }

    Then("the system exists")
    assert(exists(itSystem))
    And("the microservice exists")
    assert(exists(ms))
    And("the database exists")
    assert(exists(db))
    And("the town plan has the correct relationships")
    assert(townPlan.relationships.size == 5)
    And("the system has the correct number of containers")
    assert(townPlan.containers(itSystem).size == 2)
    And("the containers have documentation links")
    assert(townPlan.container(ms.key).exists(_.apiDocumentationLinks.nonEmpty))
    And("the containers have an API")
    assert(
      townPlan
        .container(ms.key)
        .exists(_.api.exists(api => api.style == RestAPI()))
    )
  }

}
