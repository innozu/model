package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.relationships.Association
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class RiskSpec extends AnyFlatSpec with GivenWhenThen {
  "risks" can "be added to the town plan" in new EnterpriseArchitectureContext {
    When("risks are added to the town plan")
    val system: ItSystem = ea hasRandomItSystem ()
    val microservice: Microservice =
      ea hasRandomContainer (Microservice(), { it =>
        it isPartOf system
      })
    val technicalDebt: Risk = ea hasRandomRisk { it =>
      it isAssociatedWith system
      it isAssociatedWith microservice
    }
    Then("The risks exist")
    assert(exists(technicalDebt))
    assert(townPlan.relationships.count(_.isInstanceOf[Association]) == 2)
  }
}
