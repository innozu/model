package com.innovenso.townplanner.model.concepts

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class ArchitectureBuildingBlockSpec extends AnyFlatSpec with GivenWhenThen {
  "Architecture Building Blocks" can "be added to the town plan" in new EnterpriseArchitectureContext {
    val innovenso: Enterprise = ea hasRandom (Enterprise())
    val marketing: BusinessCapability =
      ea describesRandom (BusinessCapability()) as { it =>
        it serves innovenso
      }
    val cdp: ArchitectureBuildingBlock =
      ea describesRandom (ArchitectureBuildingBlock()) as { it =>
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
