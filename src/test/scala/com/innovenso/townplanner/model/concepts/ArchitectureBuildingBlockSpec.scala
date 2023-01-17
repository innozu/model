package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties.{BeInvestedIn, Description, Title}
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class ArchitectureBuildingBlockSpec extends AnyFlatSpec with GivenWhenThen {
  "Architecture Building Blocks" can "be added to the town plan" in new EnterpriseArchitectureContext {
    val innovenso: Enterprise = samples.enterprise
    val marketing: BusinessCapability = samples.capability(Some(innovenso))
    val cdp: ArchitectureBuildingBlock =
      ea describes ArchitectureBuildingBlock() as { it =>
        it has Title("CDP")
        it should BeInvestedIn()
        it has Description("Customer Data Platform")
        it has Description(
          "a collection of software which creates a persistent, unified customer database that is accessible to other systems."
        )
        it has Description("notable vendors are Adobe, SAS, Tealium")
        it realizes marketing
      }

    assert(exists(cdp))
    assert(
      townPlan
        .architectureBuildingBlock(cdp.key)
        .exists(it => {
          it.descriptions.size == 3 && it.isToBeInvestedIn
        })
    )
  }

}
