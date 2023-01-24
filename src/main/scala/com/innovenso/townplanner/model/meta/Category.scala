package com.innovenso.townplanner.model.meta

import com.innovenso.townplanner.model.samples

case class Category(value: Option[String]) {
  override def toString: String = value.getOrElse("None")
}

object Category {
  def apply(value: Option[String]) = new Category(value)

  def random = apply(Some(samples.title))
}
