package com.innovenso.townplanner.model.meta

import com.innovenso.townplanner.model.samples

import java.time.LocalDate

case class SomeYear(value: Int) extends Year

case object ThisYear extends Year {
  val value: Int = LocalDate.now().getYear
}

trait Year {
  def value: Int

  override def toString: String = s"$value"
}

object Year {
  def random: Year = SomeYear(samples.randomInt(3000))
}
