package com.innovenso.townplanner.model.meta

import com.innovenso.townplanner.model.samples

case class UnitOfMeasure(value: String) {
  override def toString: String = value
}

object UnitOfMeasure {
  def apply(value: String): UnitOfMeasure = new UnitOfMeasure(value)

  def random: UnitOfMeasure = apply(samples.word)
}
