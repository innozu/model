package com.innovenso.townplanner.model.views

import com.innovenso.townplanner.model.concepts._
import com.innovenso.townplanner.model.concepts.properties.Description
import com.innovenso.townplanner.model.concepts.views.{CompiledFullTownPlanView, FullTownPlanView}
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class FullTownPlanViewSpec extends AnyFlatSpec with GivenWhenThen {
  "Full Town Plan Views" can "be added to the town plan" in new EnterpriseArchitectureContext {
    Given("an enterprise")
    val innovenso: Enterprise = ea has Enterprise(title = "Innovenso")
    val apple: Enterprise = ea has Enterprise(title = "Apple")

    And("some business capabilities")
    val marketing: BusinessCapability =
      ea describes BusinessCapability(title = "Marketing") as { it =>
        it has Description(
          "One of the most important capabilities of a modern enterprise"
        )
        it serves innovenso
      }

    val customerSegmentation: BusinessCapability =
      ea describes BusinessCapability(title = "Customer Segmentation") as {
        it =>
          it serves marketing
      }

    val product: BusinessCapability =
      ea describes BusinessCapability(title = "Product") as { it =>
        it serves innovenso
      }

    val productDesign: BusinessCapability =
      ea describes BusinessCapability(title = "Product Design") as { it =>
        it serves product
      }

    val productDevelopment: BusinessCapability =
      ea describes BusinessCapability(title = "Product Development") as { it =>
        it serves product
      }

    val otherEnterpriseCapability: BusinessCapability =
      ea has BusinessCapability(title = "something else")

    And("some architecture building blocks")
    val buildingBlockUnderTest: ArchitectureBuildingBlock =
      ea describes ArchitectureBuildingBlock(title = "CI/CD Platform") as {
        it =>
          it realizes productDevelopment
      }

    val otherBuildingBlock: ArchitectureBuildingBlock =
      ea describes ArchitectureBuildingBlock(title = "UX Design tool") as {
        it =>
          it realizes productDesign
      }

    And("some platforms")
    val platform1: ItPlatform =
      ea describes ItPlatform(title = "Included") as { it =>
        it realizes buildingBlockUnderTest
      }

    val platform2: ItPlatform =
      ea describes ItPlatform(title = "Excluded") as { it =>
        it realizes otherBuildingBlock
      }

    And("some systems")
    val platformSystem: ItSystem =
      ea describes ItSystem(title = "Included Platform System") as { it =>
        it isPartOf platform1
      }

    val includedSystem: ItSystem =
      ea describes ItSystem(title = "Included System") as { it =>
        it realizes buildingBlockUnderTest
      }

    val excludedSystem: ItSystem = ea has ItSystem(title = "some other system")
    And("some containers")
    val includedContainer: ItContainer =
      ea describes Microservice(title = "microservice") as { it =>
        it isPartOf includedSystem
      }

    val excludedContainer: ItContainer =
      ea describes Microservice(title = "other microservice") as { it =>
        it isPartOf excludedSystem
      }

    When("a full town plan view is requested")
    val viewUnderTest: FullTownPlanView =
      ea needs FullTownPlanView(forEnterprise = innovenso.key)

    Then("the view exists")
    assert(exists(viewUnderTest))

    val compiledViewUnderTest: CompiledFullTownPlanView =
      townPlan.fullTownPlanView(viewUnderTest.key).get

    And("it contains the correct elements")
    assert(compiledViewUnderTest.enterprise.contains(innovenso))
    assert(
      compiledViewUnderTest.businessCapabilities.contains(productDevelopment)
    )
    assert(compiledViewUnderTest.businessCapabilities.contains(product))
    assert(compiledViewUnderTest.businessCapabilities.contains(productDesign))
    assert(compiledViewUnderTest.businessCapabilities.contains(marketing))
    assert(compiledViewUnderTest.enterprises.contains(innovenso))
    assert(!compiledViewUnderTest.enterprises.contains(apple))
    assert(compiledViewUnderTest.platforms.contains(platform1))
    assert(compiledViewUnderTest.platforms.contains(platform2))
    assert(compiledViewUnderTest.systems.contains(includedSystem))
    assert(compiledViewUnderTest.systems.contains(platformSystem))
    assert(!compiledViewUnderTest.systems.contains(excludedSystem))
    assert(compiledViewUnderTest.containers.contains(includedContainer))
    assert(!compiledViewUnderTest.containers.contains(excludedContainer))
    assert(compiledViewUnderTest.relationships.nonEmpty)
  }
}
