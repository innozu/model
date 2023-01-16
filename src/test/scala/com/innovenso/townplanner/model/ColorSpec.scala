package com.innovenso.townplanner.model

import com.innovenso.townplanner.model.meta.Color
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class ColorSpec extends AnyFlatSpec with GivenWhenThen {
  "a color with an alpha value" should "be lighter" in {
    Given("a color")
    val color: Color = Color(255, 0, 0)("red")
    When("an alpha value is added")
    val withAlpha: Color = color.withAlpha(20)
    Then("the color is lighter")
    println(withAlpha)
  }
}
