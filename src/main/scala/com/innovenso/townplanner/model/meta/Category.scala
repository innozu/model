package com.innovenso.townplanner.model.meta

case class Category(value: Option[String]) {
  override def toString: String = value.getOrElse("None")
}
