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
    And("some containers in each system")
    val container11: Microservice =
      ea hasRandomContainer (Microservice(), { it =>
        it isPartOf system1
      })
    val container12: Database = ea hasRandomContainer (Database(), { it =>
      it isPartOf system1
    })
    val container21: Microservice =
      ea hasRandomContainer (Microservice(), { it =>
        it isPartOf system2
      })
    val container22: WebUI = ea hasRandomContainer (WebUI(), { it =>
      it isPartOf system2
    })
    val container31: Microservice =
      ea hasRandomContainer (Microservice(), { it =>
        it isPartOf system3
      })

    And("relationships between the containers")
    ea hasRelationship Flow(source = container21.key, target = container22.key)
    ea hasRelationship Flow(source = container11.key, target = container12.key)
    ea hasRelationship Flow(source = container21.key, target = container11.key)
    ea hasRelationship Flow(source = container21.key, target = container12.key)
    ea hasRelationship Flow(source = container21.key, target = container31.key)
    ea hasRelationship Flow(source = system2.key, target = system3.key)

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
