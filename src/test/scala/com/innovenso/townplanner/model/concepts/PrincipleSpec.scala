package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties.{Description, Title}
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class PrincipleSpec extends AnyFlatSpec with GivenWhenThen {
  "principles" can "be added to the town plan" in new EnterpriseArchitectureContext {
    Given("an enterprise")
    val innovenso: Enterprise = ea hasRandomEnterprise ()
    When("a principle is added to it")
    val doItRight: CorporatePrinciple = ea describes CorporatePrinciple() as {
      it =>
        it has Title("We do it right or we don't do it")
        it has Description("It's about focus")
        it serves innovenso
    }

    Then("the principle exists")
    assert(exists(doItRight))
    And("has a relationship with the enterprise")
    assert(townPlan.relationships.size == 1)
  }

}
