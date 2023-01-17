package com.innovenso.townplanner.model.processor.autocomplete

import com.innovenso.townplanner.model.concepts._
import com.innovenso.townplanner.model.concepts.relationships.Flow
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class SystemFlowAutocompleteProcessorSpec
    extends AnyFlatSpec
    with GivenWhenThen {
  "flows between containers of different systems" should "result in flows between the systems and the platforms" in new EnterpriseArchitectureContext {
    Given("some systems")
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
    And("some containers in each system")
    val container11: Microservice =
      samples.microservice(system1, name = Some("microservice 1"))
    val container12: Database =
      samples.database(system1, name = Some("database 1"))
    val container21: Microservice =
      samples.microservice(system2, name = Some("microservice 2"))
    val container22: WebUI = samples.ui(system2, name = Some("ui 1"))
    val container31: Microservice =
      samples.microservice(system3, name = Some("microservice 3"))

    And("relationships between the containers")
    samples.flow(container21, container22, name = Some("pushes notifications"))
    samples.flow(container11, container12, name = Some("stores data"))
    samples.flow(container21, container11, name = Some("uses"))
    samples.flow(
      container21,
      container12,
      name = Some("stores data in other microservice")
    )
    samples.flow(container21, container31, name = Some("updates"))
    samples.flow(system2, system3, name = Some("system to system"))

    And("a system flow autocomplete processor")
    val systemAutoComplete: SystemFlowAutocompleteProcessor =
      SystemFlowAutocompleteProcessor()(ea)
    When("the system flow autocomplete is run")
    process(systemAutoComplete)
    val platformAutoComplete: PlatformFlowAutocompleteProcessor =
      PlatformFlowAutocompleteProcessor()(ea)
    process(platformAutoComplete)

    Then("relationships are autocompleted")
    assert(townPlan.relationshipsWithType(classOf[Flow]).size == 15)
  }

}
