package com.innovenso.townplanner.model.meta

sealed trait Severity {
  def name: String

  override def toString: String = name
}

case object Amber extends Severity {
  val name = "Amber"
}

case object Red extends Severity {
  val name = "Red"
}

case object Green extends Severity {
  val name = "Green"
}
