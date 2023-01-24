package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties.{
  BeInvestedIn,
  Description,
  Title
}
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class ArchitectureBuildingBlockSpec extends AnyFlatSpec with GivenWhenThen {
  "Architecture Building Blocks" can "be added to the town plan" in new EnterpriseArchitectureContext {
    val innovenso: Enterprise = ea hasRandomEnterprise ()
    val marketing: BusinessCapability = ea hasRandomBusinessCapability { it =>
      it serves innovenso
    }
    val cdp: ArchitectureBuildingBlock = ea hasRandomArchitectureBuildingBlock {
      it =>
        it realizes marketing
    }

    assert(exists(cdp))
    assert(
      townPlan
        .architectureBuildingBlock(cdp.key)
        .exists(it => {
          it.descriptions.nonEmpty
        })
    )
  }

}
