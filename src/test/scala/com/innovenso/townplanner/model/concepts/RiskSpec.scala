package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.relationships.Association
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class RiskSpec extends AnyFlatSpec with GivenWhenThen {
  "risks" can "be added to the town plan" in new EnterpriseArchitectureContext {
    When("risks are added to the town plan")
    val system: ItSystem = samples.system()
    val microservice: Microservice = samples.microservice(system)
    val technicalDebt: Risk = samples.technicalDebt(
      List(system, microservice),
      Some("some technical debt")
    )
    Then("The risks exist")
    assert(exists(technicalDebt))
    assert(townPlan.relationships.count(_.isInstanceOf[Association]) == 2)
  }
}
