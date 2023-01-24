package com.innovenso.townplanner.model.meta

import com.innovenso.townplanner.model.samples

case class UnitCount(count: Double) {
  override def toString: String = f"${count}%2.2f"
}

object UnitCount {
  def apply(count: Double) = new UnitCount(count)

  def random: UnitCount = apply(samples.randomDouble(100d))
}
