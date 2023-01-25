package com.innovenso.townplanner.model.concepts

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class PlatformLayerSpec extends AnyFlatSpec with GivenWhenThen {
  "Platform Layers" can "be added to the town plan" in new EnterpriseArchitectureContext {
    When("a platform layer is added to the town plan")
    val thePlatformLayer: PlatformLayer = ea hasRandom PlatformLayer ()

    Then("the platform layer exists")
    assert(exists(thePlatformLayer))
    println(thePlatformLayer.color)
  }
}
