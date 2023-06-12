package com.innovenso.townplanner.model.meta

import java.security.SecureRandom

import org.apache.commons.lang3.StringUtils

case class Color(red: Int, green: Int, blue: Int)(val name: String) {
  val hex: String = String.format("#%02x%02x%02x", red, green, blue)
  val tuple: (String, Color) =
    ("innovenso" + StringUtils.capitalize(name), this)
  def withAlpha(alpha: Int): Color = {
    copy(
      red = applyAlpha(red, alpha),
      green = applyAlpha(green, alpha),
      blue = applyAlpha(blue, alpha)
    )(name)
  }

  private def applyAlpha(value: Int, alpha: Int): Int = {
    (255 - (alpha.toFloat / 100) * (255 - value)).toInt
  }

  override def toString: String = s"$hex ($name)"
}

object Color {
  private val secureRandom = new SecureRandom()
  private def randomInt: Int =
    secureRandom.nextInt(256)

  def random = new Color(randomInt, randomInt, randomInt)(Key().camelCased)

  def apply(red: Int, green: Int, blue: Int)(
      name: String = Key().camelCased
  ): Color =
    new Color(red, green, blue)(name)
  def apply(hex: String)(name: String): Color = new Color(
    Integer.valueOf(hex.substring(1, 3), 16),
    Integer.valueOf(hex.substring(3, 5), 16),
    Integer.valueOf(hex.substring(5, 7), 16)
  )(name)

}
