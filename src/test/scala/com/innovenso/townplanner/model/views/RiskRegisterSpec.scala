package com.innovenso.townplanner.model.views

import com.innovenso.townplanner.model.concepts.views.RiskRegister
import com.innovenso.townplanner.model.concepts._
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class RiskRegisterSpec extends AnyFlatSpec with GivenWhenThen {
  "risks" can "be seen in the risk register" in new EnterpriseArchitectureContext {
    When("risks are added to the town plan")
    val system: ItSystem = samples.system()
    val microservice: Microservice = samples.microservice(system)
    val database: Database = samples.database(system)
    val technicalDebt: Risk = samples.technicalDebt(
      List(system, microservice),
      Some("some technical debt")
    )
    And("a risk register is requested")
    val view = ea needs RiskRegister()
    val compiledView = townPlan.riskRegister.get
    Then("The risks exist")
    assert(exists(technicalDebt))
    assert(compiledView.systems.size == 1)
    assert(compiledView.containers.size == 1)
    assert(compiledView.relationships.size == 2)
  }
}
